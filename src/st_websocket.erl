-module(st_websocket).

% Exported API
-export([authorise/2, connect/3, init_socket/1, handover/4, send_data/3, send_message/2, bootstrap_child/3, get/2, get/3, set/3]).


%% ===================================================================
%% Definitions
%% ===================================================================

-record(wsstate, {socket, request, options, stream_pid = undefined, call_backs, data_buf = <<>>, payload_buf = <<>>, payload_op = 0, 
					timeout, parent_pid, data = []}).

%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Authorise a websocket request
%% ====================

authorise(_Origin, all) ->
	true;
authorise(undefined, _AllowOrigins) ->
	false;
authorise(Origin, AllowOrigins) ->
	lists:member(Origin, AllowOrigins).


%% ====================
%% Connect a web socket
%% ====================

connect(Request, Options, CallBacks) ->
	SecResponse = sec_response(st_request:get_header(Request, <<"sec-websocket-key">>, <<>>)),
	{ok, Response} = st_response:new(Request),
	{ok, WSResponse} = st_response:websocket(Response),
	{ok, StatusCodeResponse} = st_response:status_code(WSResponse, 101),
	{ok, FinalResponse} = st_response:set_headers(StatusCodeResponse,
			[{<<"Sec-WebSocket-Accept">>, SecResponse}, {<<"Upgrade">>, <<"websocket">>}, {<<"Connection">>, <<"Upgrade">>},
			{<<"Sec-WebSocket-Version">>, <<"13">>}]),
	{websocket, FinalResponse, Options, CallBacks}.

%% ====================
%% Initialise the socket
%% ====================

init_socket(Socket) ->
	st_socket:setopts(Socket, [{packet, raw}, {active, true}]).

%% ====================
%% Set the security response
%% ====================

sec_response(SecKey) ->
	DecodedAnswer = crypto:sha(<<SecKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>),
	base64:encode(DecodedAnswer).



%% ===================================================================
%% Take control of the process
%% ===================================================================

handover(Socket, Request, Options, CallBacks) ->
	init_socket(Socket),

	OrigWS = #wsstate{parent_pid = self(), socket = Socket, request = Request, options = Options, call_backs = CallBacks,
						timeout = proplists:get_value(timeout, Options, infinity),
						data = [ Val || {Opt, Val} <- Options, Opt == set ]},

	% Set up the streaming process
	WS = case proplists:get_value(stream, Options) of
		{Module, Function} ->
			Pid = spawn_link(st_websocket, bootstrap_child, [Module, Function, OrigWS]),
			OrigWS#wsstate{stream_pid = Pid};
		undefined ->
			OrigWS
	end,

	% Auto subscribe to a list of st_mq channels
	STMQChannels = proplists:get_value(stmq_subscribe, Options, []),
	[ st_mq:subscribe(Chan) || Chan <- STMQChannels ],

	% Call initialisation code
	case proplists:get_value(init, CallBacks) of
		undefined ->
			WS;
		Fun when is_function(Fun, 1) ->
			case Fun(WS) of
				ok -> main_loop(WS);
				{ok, NewWS} -> main_loop(NewWS);
				close -> terminate(WS)
			end
	end.

main_loop(WS) ->
	Timeout = WS#wsstate.timeout,
	receive
		{tcp, _Socket, Data} ->
			case receive_data(WS, Data) of
				{ok, NewWS} ->
					try
						main_loop(NewWS)
					catch
						Err:Details ->
							io:format("Error in websocket process: ~p:~p~n~p~n~n", [Err, Details, erlang:get_stacktrace()]),
							terminate(WS)
					end;
				stop ->
					terminate(WS);
				Other ->
					io:format("Unexpected return value from receive_data: ~p~n~n", [Other])
			end;

		{wssend, Op, Data} ->
			send_data(WS, Op, Data),
			main_loop(WS);

		stop ->
			send_data(WS, close, <<>>),
			terminate(WS);

		{tcp_closed, _Port} ->
			io:format("WebSocket has closed.~n"),
			terminate(WS);

		{st_mq, Msg} ->
			case proplists:get_value(stmq_msg, WS#wsstate.options, undefined) of
				auto_forward ->
					send_message(WS, st_mq:msg_data(Msg)),
					main_loop(WS);
				undefined ->
					case proplists:get_value(msg, WS#wsstate.call_backs) of
						Fun when is_function(Fun) ->
							case Fun(WS, Msg) of
								ok ->
									main_loop(WS);
								{ok, NewWS} ->
									main_loop(NewWS);
								stop ->
									stop
							end;
						undefined ->
							io:format("~n>>> Unexpected st_mq message in websocket main loop: ~p~n~n", [Msg]),
							main_loop(WS)
					end
			end;

		Msg ->
			case proplists:get_value(msg, WS#wsstate.call_backs) of
				Fun when is_function(Fun) ->
					case Fun(WS, Msg) of
						ok ->
							main_loop(WS);
						{ok, NewWS} ->
							main_loop(NewWS);
						stop ->
							stop
					end;
				undefined ->
					io:format("~n>>> Unexpected message in websocket main loop: ~p~n~n", [Msg]),
					main_loop(WS)
			end
	after
		Timeout ->
			terminate(WS)
	end.

terminate(WS) ->
	if WS#wsstate.stream_pid == undefined -> ok;
	true -> exit(WS#wsstate.stream_pid, kill) end,
	stop.

send_data(WS, Op, Payload) ->
	OpCode = case Op of
		text -> 1;
		binary -> 2;
		close -> 8;
		ping -> 9;
		pong -> 10
	end,
	Encoded = if
		byte_size(Payload) >= 65536 -> <<1:1, 0:3, OpCode:4, 0:1, 127:7, (byte_size(Payload)):64, Payload/binary>>;
		byte_size(Payload) >= 126	-> <<1:1, 0:3, OpCode:4, 0:1, 126:7, (byte_size(Payload)):16, Payload/binary>>;
		true						-> <<1:1, 0:3, OpCode:4, 0:1, 		 (byte_size(Payload)):7, Payload/binary>>
	end,
	st_socket:send(WS#wsstate.socket, Encoded).

send_message(WS, Msg) ->
	case proplists:get_value(stmq_format, WS#wsstate.options) of
		raw ->
			send_data(WS, text, st_mq:msg_data(Msg));
		jquery ->
			JSON = {struct, [
				{<<"type">>, <<"message">>},
				{<<"data">>, Msg}
			]},
			send_data(WS, text, stutil:to_binary(json:encode(JSON)))
	end.


receive_data(WS, NewData) ->
	NewWS = WS#wsstate{data_buf = <<(WS#wsstate.data_buf)/binary, NewData/binary>>},
	process_data_buffer(NewWS).

process_data_buffer(WS) ->
	case WS#wsstate.data_buf of
		<<Fin:1, _Rsv:3, OpCode:4, Mask:1, PayloadLen:7, Rest/binary>> ->
			process_payload_len(WS, Fin, OpCode, Mask, PayloadLen, Rest);
		_ ->
			{ok, WS}
	end.

process_payload_len(WS, Fin, OpCode, Mask, PayloadLen, Data) when PayloadLen =< 125 ->
	process_mask_key(WS, Fin, OpCode, Mask, PayloadLen, Data);
process_payload_len(WS, Fin, OpCode, Mask, PayloadLen, Data) when PayloadLen == 126 ->
	<<NewPayloadLen:16/big, Rest/binary>> = Data,
	process_mask_key(WS, Fin, OpCode, Mask, NewPayloadLen, Rest);
process_payload_len(WS, Fin, OpCode, Mask, PayloadLen, Data) when PayloadLen == 127 ->
	<<NewPayloadLen:64/big, Rest/binary>> = Data,
	process_mask_key(WS, Fin, OpCode, Mask, NewPayloadLen, Rest).

process_mask_key(WS, Fin, OpCode, Mask, PayloadLen, Data) when Mask == 1 ->
	<<MaskKey:4/binary, Rest/binary>> = Data,
	process_payload(WS, Fin, OpCode, MaskKey, PayloadLen, Rest);
process_mask_key(_WS, _Fin, _OpCode, Mask, _PayloadLen, _Data) when Mask == 0 ->
	stop.

process_payload(WS, Fin, OpCode, MaskKey, PayloadLen, Data) ->
	if byte_size(Data) < PayloadLen ->
		io:format("Websocket:  Not enough data ~p vs ~p...~n~p~n~n", [byte_size(Data), PayloadLen, Data]),
		{ok, WS};
	true ->
		Payload = binary:part(Data, 0, PayloadLen),
		LeftOver = binary:part(Data, PayloadLen, byte_size(Data) - PayloadLen),
		process_frame(WS#wsstate{data_buf = LeftOver}, Fin, OpCode, apply_mask(Payload, MaskKey))
	end.

apply_mask(Payload, undefined) ->
	Payload;
apply_mask(Payload, Key) ->
	apply_mask(Payload, Key, 0, <<>>).

apply_mask(<<Byte, Payload/binary>>, Key, I, Acc) ->
	<<MaskByte>> = binary:part(Key, I rem 4, 1),
	TransByte = Byte bxor MaskByte,
	apply_mask(Payload, Key, I + 1, <<Acc/binary, TransByte>>);
apply_mask(<<>>, _Key, _I, Acc) ->
	Acc.


process_frame(_WS, _Fin, 8, _Payload) ->
	stop;
process_frame(WS, _Fin, 9, Payload) ->
	send_data(WS, pong, Payload),
	ok;
process_frame(WS, Fin, OpCode, Payload) ->
	NewWS = WS#wsstate{payload_buf = <<(WS#wsstate.payload_buf)/binary, Payload/binary>>,
				payload_op = if OpCode == 0 -> WS#wsstate.payload_op; true -> OpCode end},
	if Fin == 1 ->
		process_message(NewWS);
	true ->
		process_data_buffer(NewWS)
	end.

process_message(OrigWS) ->
	Payload = OrigWS#wsstate.payload_buf,
	WS = OrigWS#wsstate{payload_buf = <<>>},

	case proplists:get_value(rx, WS#wsstate.call_backs) of
		Fun when is_function(Fun) ->
			case Fun(WS, Payload) of
				ok ->
					process_data_buffer(WS);
				{ok, NewWS} ->
					process_data_buffer(NewWS);
				stop ->
					stop
			end;
		undefined ->
			process_data_buffer(WS)
	end.

bootstrap_child(Module, Function, WS) ->
	process_flag(trap_exit, false),
	apply(Module, Function, [WS]).


%%%%%%%%%%%%%%%%%%%%%%%%%

set(WS, Key, Val) ->
	WS#wsstate{data = set_key(Key, Val, WS#wsstate.data, [])}.

set_key(Key, Val, [{Key, _V} | Rest], Acc) ->
	[{Key, Val} | Acc] ++ Rest;
set_key(Key, Val, [KVP | Rest], Acc) ->
	set_key(Key, Val, Rest, [KVP | Acc]);
set_key(Key, Val, [], Acc) ->
	[{Key, Val} | Acc].


get(WS, Key) ->
	proplists:get_value(Key, WS#wsstate.data).
	
get(WS, Key, Default) ->
	proplists:get_value(Key, WS#wsstate.data, Default).
	
