-module(stampede_transport).

-behaviour(gen_server).

% API
-export([start_link/4]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(transstate, {rec_state, parent_pool, server_socket, socket, routing_rules, options,
						timeout_header, timeout_keepalive,
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
	io:format("Worker on socket ~s initialised.~n", [st_socket:name(ServerSocket)]),
	State = #transstate{rec_state = accept, parent_pool = ParentPool, server_socket = ServerSocket,
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
	{ok, NewRequest} = st_request:header(State#transstate.request, Header, Value),
    st_socket:active_once(State#transstate.socket),
    NewState = State#transstate{request = NewRequest},
    {noreply, NewState, NewState#transstate.timeout_header};

% Received the end of the headers
handle_info({http, _Sock, http_eoh}, State) when State#transstate.rec_state == headers ->
	{ok, NewRequest} = st_request:end_headers(State#transstate.request, State#transstate.socket),
    NewState = State#transstate{request = NewRequest},
    {noreply, NewState, NewState#transstate.timeout_header};

% New request started
handle_info({http, _Sock, {http_request, Method, {abs_path, Path}, HttpVersion}}, State) when State#transstate.rec_state == request ->
	{ok, NewRequest} = st_request:new(Method, Path, HttpVersion),
    st_socket:active_once(State#transstate.socket),
    NewState = State#transstate{request = NewRequest, rec_state = headers},
    {noreply, NewState, NewState#transstate.timeout_header};

% Initialisation trigger...  create the initial worker pool
handle_info(timeout, #transstate{rec_state = accept, server_socket = ServerSocket, parent_pool = Parent} = State) ->
    {ok, Socket} = st_socket:accept(ServerSocket),
    
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

    NewState = State#transstate{rec_state = request, socket = Socket},
    {noreply, NewState, NewState#transstate.timeout_header};

handle_info(timeout, State) ->
	io:format("Request timed out.~n"),
	{stop, normal, State};

handle_info(Msg, State) ->
	io:format("Unexpected info to stampede_transport: ~p, ~p~n", [Msg, State]),
	{noreply, State}.


%% =========================
%% Terminate
%% =========================

terminate(_Reason, _State) ->
	ok.


%% =========================
%% Code Change
%% =========================

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ===================================================================
%% Support functions
%% ===================================================================

