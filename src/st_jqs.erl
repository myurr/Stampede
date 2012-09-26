-module(st_jqs).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([register_socket/2, socket_to_pid/1, unregister_socket/1, unregister_pid/1]).
-export([init/0, new_websocket/5, new_stream/6, ws_init/1, ws_rx/2, send/2, get/2, get/3, set/3,
			stream_handover/3, stream_post/1]).


%% ===================================================================
%% Definitions
%% ===================================================================

-record(jqs_state, {}).

-record(jqs, {type, socket_id, response, sock, state, call_backs, options, request, ws, data = [], padding = 0}).

-record(jqs_socket, {id, pid}).

%% ===================================================================
%% Simple server to handle exit signals to keep jqs_socket tidy
%% ===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	process_flag(trap_exit, true),
	State = #jqs_state{},
	{ok, State}.

handle_call(Request, From, State) ->
	io:format("Unexpected call to st_jqs: ~p, ~p, ~p~n", [Request, From, State]),
	{reply, ok, State}.

handle_cast({register, Pid}, State) ->
	case link(Pid) of
		true -> ok;
		noproc ->
			unregister_pid(Pid)
	end,
	{noreply, State};
handle_cast(Request, State) ->
	io:format("Unexpected cast to st_jqs: ~p, ~p~n", [Request, State]),
	{noreply, State}.


handle_info({'EXIT', Pid, _Reason}, State) ->
	unregister_pid(Pid),
	{noreply, State};
handle_info(Msg, State) ->
	io:format("Unexpected info to st_jqs: ~p, ~p~n", [Msg, State]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%% ===================================================================
%% API functions
%% ===================================================================

init() ->
	{atomic, ok} = mnesia:create_table(jqs_socket, [
									{attributes, record_info(fields, jqs_socket)},
									{type, set}, 
									{index, [#jqs_socket.pid]}, 
									{record_name, jqs_socket}
								]),
	ok.

register_socket(SocketId, Pid) ->
	F = fun() ->
		case mnesia:read(jqs_socket, SocketId) of
			[] ->
				mnesia:write(jqs_socket, #jqs_socket{id = SocketId, pid = Pid}, write);
			[Existing] ->
				Existing#jqs_socket.pid ! {'EXIT', Existing#jqs_socket.pid, normal},
				timer:kill_after(5000, Pid),
				mnesia:write(jqs_socket, #jqs_socket{id = SocketId, pid = Pid}, write)
		end,
		gen_server:cast(?MODULE, {register, Pid})
	end,
	mnesia:activity(transaction, F).

socket_to_pid(SocketId) ->
	F = fun() ->
		case mnesia:read(jqs_socket, SocketId) of
			[] -> undefined;
			[Sock] -> Sock#jqs_socket.pid
		end
	end,
	mnesia:activity(transaction, F).

unregister_socket(SocketId) ->
	mnesia:activity(transaction, fun() -> mnesia:delete(jqs_socket, SocketId, write) end).

unregister_pid(Pid) ->
	io:format("Unregistering socket for PID ~p~n", [Pid]),
	F = fun() ->
		case mnesia:index_read(jqs_socket, Pid, #jqs_socket.pid) of
			[] -> undefined;
			[Sock] -> mnesia:delete(jqs_socket, Sock#jqs_socket.id, write)
		end
	end,
	mnesia:activity(transaction, F).



new_websocket(SocketId, Request, Options, CallBacks, AllowOrigin) ->
	% Check the origin is authorised
	Authorised = st_websocket:authorise(st_request:get_header(Request, <<"origin">>, undefined), AllowOrigin),
	if Authorised ->
		Jqs = #jqs{type = websocket, socket_id = SocketId, state = undefined, call_backs = CallBacks, options = Options, request = Request},
		register_socket(SocketId, self()),
		st_websocket:connect(Request, [{stmq_format, jquery}, {set, {jqs, Jqs}}] ++ Options, [{init, fun st_jqs:ws_init/1}, {rx, fun st_jqs:ws_rx/2}]);
	true ->
		io:format("JQuery socket invalid origin (ws): ~p~n", [st_request:get_header(Request, <<"origin">>, undefined)]),
		{error, 403, <<"Invalid Origin">>}
	end.

new_stream(Type, SocketId, Request, Options, CallBacks, AllowOrigin) ->
	% Check the origin is authorised
	Authorised = stream_authorise(Type, st_request:get_header(Request, <<"origin">>, undefined), AllowOrigin),
	if Authorised ->
		Jqs = #jqs{type = Type, state = undefined, socket_id = SocketId, call_backs = CallBacks, options = Options, request = Request},
		register_socket(SocketId, self()),
		stream_connect(Jqs);
	true ->
		io:format("JQuery socket invalid origin (~p): ~p~n", [Type, st_request:get_header(Request, <<"origin">>, undefined)]),
		{error, 403, <<"Invalid Origin">>}
	end.


stream_authorise(sse, _Origin, _Allow) ->
	true;
stream_authorise(streamiframe, _Origin, _Allow) ->
	true;
stream_authorise(streamxhr, _Origin, _Allow) ->
	true;
stream_authorise(_Type, _Origin, all) ->
	true;
stream_authorise(_Type, undefined, _AllowOrigin) ->
	false;
stream_authorise(_Type, Origin, AllowOrigin) ->
	lists:member(Origin, AllowOrigin).

stream_connect(Jqs) ->
	Headers = if Jqs#jqs.type == streamxhr; Jqs#jqs.type == sse; Jqs#jqs.type == streamiframe ->
		[];
	true ->
		[ {<<"Access-Control-Allow-Origin">>, st_request:get_header(Jqs#jqs.request, <<"origin">>, <<"*">>)} ]
	end,
	Padding = case st_request:user_agent(Jqs#jqs.request) of
		undefined -> 4096;
		UA ->
			case binary:matches(UA, <<"Android 2.">>) of
				[] ->
					case binary:matches(UA, <<"Android 3.">>) of
						[] -> 0;
						_ -> 4096
					end;
				_ -> 4096
			end
	end,
	{ok, NewResponse} = st_response:new(Jqs#jqs.request, 200, Headers, {stream, <<(binary:copy(<<" ">>, 4096))/binary, 10>>}),
	Response = st_response:content_type(NewResponse, if Jqs#jqs.type == sse -> <<"text/event-stream">>; true -> <<"text/plain">> end),
	{handover, Response, fun st_jqs:stream_handover/3, Jqs#jqs{padding = Padding}}.



send(Jqs, Msg) ->
	JSON = {struct, [
		{<<"type">>, <<"message">>},
		{<<"data">>, Msg}
	]},
	case Jqs#jqs.type of
		websocket ->
			st_websocket:send_data(Jqs#jqs.ws, text, stutil:to_binary(json:encode(JSON)));
		streamxdr ->
			DataLines = [ <<"data: ", L/binary>> || L <- stutil:split_lines(stutil:to_binary(json:encode(JSON))) ],
			Data = <<(binary:copy(<<" ">>, Jqs#jqs.padding))/binary, (stutil:binary_join(DataLines, <<10>>))/binary, 10, 10>>,
			st_socket:send(Jqs#jqs.sock, st_response:encode_chunk(Data));
		streamxhr ->
			DataLines = [ <<"data: ", L/binary>> || L <- stutil:split_lines(stutil:to_binary(json:encode(JSON))) ],
			Data = <<(binary:copy(<<" ">>, Jqs#jqs.padding))/binary, (stutil:binary_join(DataLines, <<10>>))/binary, 10, 10>>,
			st_socket:send(Jqs#jqs.sock, st_response:encode_chunk(Data));
		sse ->
			DataLines = [ <<"data: ", L/binary>> || L <- stutil:split_lines(stutil:to_binary(json:encode(JSON))) ],
			Data = <<(binary:copy(<<" ">>, Jqs#jqs.padding))/binary, (stutil:binary_join(DataLines, <<10>>))/binary, 10, 10>>,
			st_socket:send(Jqs#jqs.sock, st_response:encode_chunk(Data));
		streamiframe ->
			DataLines = [ <<"data: ", L/binary>> || L <- stutil:split_lines(stutil:to_binary(json:encode(JSON))) ],
			Data = <<(binary:copy(<<" ">>, Jqs#jqs.padding))/binary, (stutil:binary_join(DataLines, <<10>>))/binary, 10, 10>>,
			st_socket:send(Jqs#jqs.sock, st_response:encode_chunk(Data))
	end.


get(Jqs, Key) ->
	proplists:get_value(Key, Jqs#jqs.data).

get(Jqs, Key, Default) ->
	proplists:get_value(Key, Jqs#jqs.data, Default).

set(Jqs, Key, Val) ->
	Jqs#jqs{data = set_key(Key, Val, Jqs#jqs.data, [])}.


%% ===================================================================
%% Websocket callbacks
%% ===================================================================

ws_init(WS) ->
	OrigJqs = st_websocket:get(WS, jqs),
	Jqs = OrigJqs#jqs{ws = WS},
	Fun = proplists:get_value(init, Jqs#jqs.call_backs),
	if Fun == undefined -> ok;
	true ->
		case Fun(Jqs) of
			ok -> {ok, st_websocket:set(WS, jqs, Jqs)};
			{ok, NewJqs} -> {ok, st_websocket:set(WS, jqs, NewJqs)};
			stop -> stop
		end
	end.

ws_rx(WS, Msg) ->
	OrigJqs = st_websocket:get(WS, jqs),
	Jqs = OrigJqs#jqs{ws = WS},

	Json = json:decode(Msg),

	Fun = proplists:get_value(rx, Jqs#jqs.call_backs),
	if Fun == undefined -> ok;
	true ->
		case Fun(Jqs, stjp:get(Json, <<"data">>, <<>>)) of
			ok -> {ok, st_websocket:set(WS, jqs, Jqs)};
			{ok, NewJqs} -> {ok, st_websocket:set(WS, jqs, NewJqs)};
			stop -> stop
		end
	end.



%% ===================================================================
%% Stream callbacks
%% ===================================================================

stream_handover(Socket, Request, OrigJqs) ->
	Jqs = OrigJqs#jqs{sock = Socket, request = Request},
	st_socket:setopts(Socket, [{packet, raw}, {active, true}]),

	% Auto subscribe to a list of st_mq channels
	STMQChannels = proplists:get_value(stmq_subscribe, Jqs#jqs.options, []),
	[ st_mq:subscribe(Chan) || Chan <- STMQChannels ],

	% Call initialisation code
	Fun = proplists:get_value(init, Jqs#jqs.call_backs),
	if Fun == undefined -> stream_main_loop(Jqs);
	true ->
		case Fun(Jqs) of
			ok -> stream_main_loop(Jqs);
			{ok, NewJqs} -> stream_main_loop(NewJqs);
			stop -> stop
		end
	end.

stream_main_loop(Jqs) ->
	receive
		{tcp, _Socket, Data} ->
			io:format("JQuery Socket received unexpected socket data (~p byte(s)).~n", [byte_size(Data)]),
			stream_main_loop(Jqs);

		{tcp_closed, _Port} ->
			terminate(Jqs);

		{post_request, Msg} ->
			Fun = proplists:get_value(rx, Jqs#jqs.call_backs),
			if Fun == undefined -> ok;
			true ->
				case Fun(Jqs, Msg) of
					ok -> stream_main_loop(Jqs);
					{ok, NewJqs} -> stream_main_loop(NewJqs);
					stop -> stop
				end
			end;

		{st_mq, Msg} ->
			send(Jqs, st_mq:msg_data(Msg)),
			stream_main_loop(Jqs);

		Msg ->
			io:format("~n>>> Unexpected message in JQuery Socket main loop: ~p~n~n", [Msg]),
			stream_main_loop(Jqs)
	end.

terminate(_Jqs) ->
	stop.

stream_post(Request) ->
	<<"data=", PostData/binary>> = st_request:post_data(Request),
	Json = json:decode(PostData),
	SocketId = stjp:get(Json, <<"socket">>),
	if SocketId == undefined ->
		{error, 400, <<"A socket id must be supplied when connecting a jquery socket.">>};
	true ->
		Pid = socket_to_pid(SocketId),				
		if Pid == undefined -> ok;
		true ->
			Pid ! {post_request, stjp:get(Json, <<"data">>, <<>>)}
		end,

		{ok, Response} = st_response:new(Request, 200, [], <<>>),
		{send, Response}
	end.



%% ===================================================================
%% Support functions
%% ===================================================================

set_key(Key, Val, [{Key, _V} | Rest], Acc) ->
	[{Key, Val} | Acc] ++ Rest;
set_key(Key, Val, [KVP | Rest], Acc) ->
	set_key(Key, Val, Rest, [KVP | Acc]);
set_key(Key, Val, [], Acc) ->
	[{Key, Val} | Acc].


