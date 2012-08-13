-module(stampede_transport).

-behaviour(gen_server).

% Definitions and constants
-define(MAX_INTERNAL_REDIRECT, 2).

% API
-export([start_link/4]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(transstate, {rec_state, x_state, parent_pool, server_socket, socket, routing_rules, options,
						timeout_header, timeout_keepalive, redirect_count,
						request}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ParentPool, ServerSocket, RoutingRules, Options) ->
	gen_server:start_link(?MODULE, [ParentPool, ServerSocket, RoutingRules, Options], []).


%% ===================================================================
%% Gen_server callbacks
%% ===================================================================


%% =========================
%% Initialisation
%% =========================

init([ParentPool, ServerSocket, RoutingRules, Options]) ->
	% io:format("Worker on socket ~s initialised.~n", [st_socket:name(ServerSocket)]),
	State = #transstate{rec_state = accept, x_state = error, 
							parent_pool = ParentPool, server_socket = ServerSocket,
							routing_rules = RoutingRules, options = Options,
							timeout_header = proplists:get_value(timeout_header, Options, 30000)},
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
	{ok, NewRequest} = st_request:new(State#transstate.socket, Method, Path, HttpVersion),
    st_socket:active_once(State#transstate.socket),
    NewState = State#transstate{request = NewRequest, rec_state = headers},
    {noreply, NewState, NewState#transstate.timeout_header};

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
    	{reuseaddr, true}
    ]),

    NewState = reset_request(State#transstate{socket = Socket}, false),
    {noreply, NewState, NewState#transstate.timeout_header};

% Timeout waiting for client
handle_info(timeout, State) ->
	{stop, normal, State};

% Connection closed by the client
handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

handle_info(Msg, State) ->
	io:format("Unexpected info to stampede_transport: ~p, ~p~n", [Msg, State]),
	{noreply, State}.


%% =========================
%% Terminate
%% =========================

terminate(_Reason, State) ->
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
 %   try
 	case
    	st_request:execute(State#transstate.request, State#transstate.routing_rules)
    of
    	{send, Response} ->
    		{ok, Data, AdditionalContent, KeepAlive} = st_response:output_response(Response),
            % io:format("Sending:~n~n~p~n~n", [Data]),
    		ok = st_socket:send(State#transstate.socket, Data),
    		case AdditionalContent of
    			undefined ->
    				ok;
    			{file, Fd} ->
	    			st_socket:send_file(State#transstate.socket, Fd),
	    			file:close(Fd)
    		end,
    		case KeepAlive of
    			false ->
    				{stop, normal, State};
    			_ ->
		    		NewState = reset_request(State, true),
    				{noreply, NewState, KeepAlive * 1000}
    		end;
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
	if ResetSocket == true -> st_socket:active_once(State#transstate.socket); true -> ok end,
	State#transstate{rec_state = request, x_state = rec, request = undefined, redirect_count = 0}.

