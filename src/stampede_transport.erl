-module(stampede_transport).

-behaviour(gen_server).

% Definitions and constants
-define(MAX_INTERNAL_REDIRECT, 2).

% API
-export([start_link/5]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(transstate, {rec_state, x_state, parent_pool, server_socket, socket, routing_rules, options,
						timeout_header, timeout_keepalive, redirect_count, site_definitions, 
						request, response}).

-define(CACHE_MAX_WAIT_TIME, 30000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ParentPool, ServerSocket, RoutingRules, SiteDefinitions, Options) ->
	gen_server:start_link(?MODULE, [ParentPool, ServerSocket, RoutingRules, SiteDefinitions, Options], []).


%% ===================================================================
%% Gen_server callbacks
%% ===================================================================


%% =========================
%% Initialisation
%% =========================

init([ParentPool, ServerSocket, RoutingRules, SiteDefinitions, Options]) ->
	% io:format("Worker on socket ~s initialised.~n", [st_socket:name(ServerSocket)]),
	State = #transstate{rec_state = accept, x_state = error, site_definitions = SiteDefinitions,
							parent_pool = ParentPool, server_socket = ServerSocket,
							routing_rules = RoutingRules, options = Options,
							timeout_header = proplists:get_value(timeout_header, Options, 30000)},
    process_flag(trap_exit, false),
	{ok, State, 0}.


%% =========================
%% Handle Call
%% =========================

handle_call(Request, From, State) ->
	io:format("Unexpected call to stampede_transport: ~p, ~p, ~p~n", [Request, From, State]),
	{reply, ok, State}.


%% =========================
%% Handle Cast
%% =========================

handle_cast(Request, State) ->
	io:format("Unexpected cast to stampede_transport: ~p, ~p~n", [Request, State]),
	{noreply, State}.


%% =========================
%% Handle Info
%% =========================

% Connection closed by the client
handle_info({tcp_closed, _Socket}, State) ->
    % io:format("Connection closed~n"),
    {stop, normal, State};


% Receive an HTTP header
handle_info({http, _Sock, {http_header, _, Header, _, Value}}, State) when State#transstate.rec_state == headers ->
	% io:format("Header: ~p = ~p~n", [Header, Value]),
	{ok, NewRequest} = st_request:header(State#transstate.request, Header, Value),
    st_socket:active_once(State#transstate.socket),
    NewState = State#transstate{request = NewRequest},
    {noreply, NewState, NewState#transstate.timeout_header};

% Received the end of the headers
handle_info({http, _Sock, http_eoh}, State) when State#transstate.rec_state == headers ->
	{ok, NewRequest} = st_request:end_headers(State#transstate.request),
    NewState = State#transstate{request = NewRequest},
    process_request(NewState);

% New request started
handle_info({http, _Sock, {http_request, Method, {abs_path, Path}, HttpVersion}}, State) when State#transstate.rec_state == request ->
    % io:format("New request started~n"),
	{ok, NewRequest} = st_request:new(State#transstate.socket, Method, Path, HttpVersion),
    st_socket:active_once(State#transstate.socket),
    NewState = State#transstate{request = NewRequest, rec_state = headers},
    {noreply, NewState, NewState#transstate.timeout_header};

% New chunk to stream
handle_info({stream, ChunkData}, State) when State#transstate.rec_state == stream ->
    % io:format("Streaming...~n"),
    st_socket:send(State#transstate.socket, st_response:encode_chunk(State#transstate.response, ChunkData)),
    {noreply, State};

% End chunk streaming
handle_info(stream_end, State) when State#transstate.rec_state == stream ->
    st_socket:send(State#transstate.socket, st_response:last_chunk(State#transstate.response)),
    case st_request:keepalive(State#transstate.request) of
        false ->
            {stop, normal, state};
        KeepAlive ->
            NewState = reset_request(State, true),
            {noreply, NewState, KeepAlive * 1000}
    end;

% Initialisation trigger...  create the initial worker pool
handle_info(timeout, #transstate{rec_state = accept, server_socket = ServerSocket, parent_pool = Parent} = State) ->
    {ok, Socket} = st_socket:accept(ServerSocket),

    unlink(Parent),
    stampede_listener:connection_accepted(Parent),

    st_socket:setopts(Socket, [
    	{active, once}, 
    	{send_timeout, proplists:get_value(tcp_send_timeout, State#transstate.options, 30000)},
    	{send_timeout_close, true},
    	{keepalive, true},
    	{delay_send, false},
    	{nodelay, true},
        {packet, http_bin},
    	{reuseaddr, true}
    ]),

    NewState = reset_request(State#transstate{socket = Socket}, false),
    {noreply, NewState, NewState#transstate.timeout_header};

% Timeout waiting for client
handle_info(timeout, State) ->
	{stop, normal, State};

% Received information from the cache
handle_info({cache_content, CI}, State) ->
    % io:format("Received cache content: ~p~n", [CI]),
    case st_response:new(State#transstate.request, CI) of
        {ok, Response} ->
            handle_request(State, {send, Response});
        {error, StatusCode, Detail} ->
            handle_request(State, {error, StatusCode, Detail})
    end;

handle_info(Msg, State) ->
	io:format("Unexpected info to stampede_transport: ~p, ~p~n", [Msg, State]),
	{stop, normal, State}.


%% =========================
%% Terminate
%% =========================

terminate(_Reason, State) ->
    % io:format("Closing thread.~n"),
	st_socket:close(State#transstate.socket),
	ok.


%% =========================
%% Code Change
%% =========================

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ===================================================================
%% Support functions
%% ===================================================================

process_request(State) when State#transstate.redirect_count >= ?MAX_INTERNAL_REDIRECT ->
	SCStr = stutil:http_status_code(error),
	ErrorHtml = <<"<html><head><title>Error ", SCStr/binary, "</title></head>",
					"<body><h1>", SCStr/binary, "</h1><div>Maximum internal redirect limit reached.</div></html>">>,
	{ok, Response} = st_response:new(State#transstate.request, error, [], ErrorHtml),
	{ok, Data, _KeepAlive} = st_response:output_response(Response),
	ok = st_socket:send(State#transstate.socket, Data),
	{stop, normal, State};



process_request(State) ->
	% io:format("Request: ~p~n", [State#transstate.request]),
    handle_request(State, st_request:execute(State#transstate.request, State#transstate.routing_rules, State#transstate.site_definitions)).

handle_request(State, Output) ->
 %   try
 	case
    	Output
    of
        {cache_hit, SaveRequest, CI} ->
            % io:format("Receive cache hit ~p~n", [CI]),
            FinalRequest = st_request:discard_post_data(st_request:save_session(SaveRequest), 30000),
            st_socket:active_once(State#transstate.socket),
            SaveState = State#transstate{rec_state = cache_async, request = FinalRequest},
            case st_response:new(FinalRequest, CI) of
                {ok, Response} ->
                    handle_request(SaveState, {send, Response});
                {error, StatusCode, Detail} ->
                    handle_request(SaveState, {error, StatusCode, Detail})
            end;

        {cache_async, SaveRequest, _Pid} ->
            FinalRequest = st_request:discard_post_data(st_request:save_session(SaveRequest), 30000),
            st_socket:active_once(State#transstate.socket),
            {noreply, State#transstate{rec_state = cache_async, request = FinalRequest}, ?CACHE_MAX_WAIT_TIME};

        {handover, Response, CallBack, Arg} ->
			{ok, Data, _AdditionalContent, _KeepAlive} = st_response:output_response(Response),
            % io:format("Sending:~n~n~p~n~n", [Data]),
			ok = st_socket:send(State#transstate.socket, Data),

		    FinalRequest = st_request:discard_post_data(st_request:save_session(st_response:request(Response)), 30000),
		    CallBack(State#transstate.socket, FinalRequest, Arg),
    		{stop, normal, State};        

        {websocket, Response, WSOpt, WSCall} ->
			{ok, Data, _AdditionalContent, _KeepAlive} = st_response:output_response(Response),
            % io:format("Sending:~n~n~p~n~n", [Data]),
			ok = st_socket:send(State#transstate.socket, Data),

		    FinalRequest = st_request:discard_post_data(st_request:save_session(st_response:request(Response)), 30000),
		    st_websocket:handover(State#transstate.socket, FinalRequest, WSOpt, WSCall),
    		{stop, normal, State};

    	{send, Response} ->
    		{ok, Data, AdditionalContent, KeepAlive} = st_response:output_response(Response),
             %io:format("Sending:~n~n~p~n~n", [Data]),
    		ok = st_socket:send(State#transstate.socket, Data),
    		case AdditionalContent of
    			undefined ->
    				ok;
    			{file, Fd} ->
	    			st_socket:send_file(State#transstate.socket, Fd),
	    			file:close(Fd)
    		end,
    		case KeepAlive of
                stream ->
                    FinalRequest = st_request:discard_post_data(st_request:save_session(st_response:request(Response)), 30000),
                    st_socket:active_once(State#transstate.socket),
                    {noreply, State#transstate{rec_state = stream, x_state = stream, response = Response, request = FinalRequest}};
    			false ->
                    st_request:save_session(st_response:request(Response)),
    				{stop, normal, State};
    			_ ->
                    st_request:discard_post_data(st_request:save_session(st_response:request(Response)), 30000),
		    		NewState = reset_request(State, true),
    				{noreply, NewState, KeepAlive * 1000}
    		end;

    	stop ->
    		{stop, normal, State};

        {error, StatusCode, Detail} ->
    		Request = st_request:error_request(State#transstate.request, StatusCode, <<"Error">>, Detail),
    		process_request(State#transstate{request = Request, redirect_count = State#transstate.redirect_count + 1});

    	error ->
    		Request = st_request:error_request(State#transstate.request, 500, <<"Error">>, <<"Unknown">>),
    		process_request(State#transstate{request = Request, redirect_count = State#transstate.redirect_count + 1})
    % catch
    %  	Error:Reason ->
    %         ErrMsg = stutil:to_binary(io_lib:format("~p : ~p", [Error, Reason])),
    %         Stack = stutil:to_binary(io_lib:format("<pre>~p</pre>", [erlang:get_stacktrace()])),
    %         Request = st_request:error_request(State#transstate.request, 500, ErrMsg, Stack),
    %         process_request(State#transstate{request = Request, redirect_count = State#transstate.redirect_count + 1})
    end.

reset_request(State, ResetSocket) ->
    if ResetSocket ->
        st_socket:setopts(State#transstate.socket, [{packet, http_bin}, {active, once}]);
    true ->
        ok
    end,
	State#transstate{rec_state = request, x_state = rec, request = undefined, response = undefined, redirect_count = 0}.
