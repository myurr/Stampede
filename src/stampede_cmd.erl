-module(stampede_cmd).

-behaviour(gen_server).

% API
-export([start_link/5]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(transstate, {procstate, parent_pool, server_socket, socket, site_definitions, options, inactive_timeout, buffer}).

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

init([ParentPool, ServerSocket, _RoutingRules, SiteDefinitions, Options]) ->
	% io:format("Worker on socket ~s initialised.~n", [st_socket:name(ServerSocket)]),
	State = #transstate{procstate = accept, site_definitions = SiteDefinitions,
							parent_pool = ParentPool, server_socket = ServerSocket,
							options = Options,
							inactive_timeout = proplists:get_value(inactive_timeout, Options, 30000)},
    process_flag(trap_exit, false),
	{ok, State, 0}.


%% =========================
%% Handle Call
%% =========================

handle_call(Request, From, State) ->
	io:format("Unexpected call to stampede_transport: ~p, ~p, ~p~n", [Request, From, State]),
	{reply, ok, State, State#transstate.inactive_timeout}.


%% =========================
%% Handle Cast
%% =========================

handle_cast(Request, State) ->
	io:format("Unexpected cast to stampede_transport: ~p, ~p~n", [Request, State]),
	{noreply, State, State#transstate.inactive_timeout}.


%% =========================
%% Handle Info
%% =========================

% Connection closed by the client
handle_info({tcp_closed, _Socket}, State) ->
    % io:format("Connection closed~n"),
    {stop, normal, State};


% More data for me, yum yum yum
handle_info({tcp, _Socket, Data}, State) ->
    io:format("Received data: ~p~n", [Data]),
    {noreply, State, State#transstate.inactive_timeout};


handle_info(timeout, #transstate{procstate = accept, server_socket = ServerSocket, parent_pool = Parent} = State) ->
    {ok, Socket} = st_socket:accept(ServerSocket),

    unlink(Parent),
    stampede_listener:connection_accepted(Parent),

    st_socket:setopts(Socket, [
    	{active, true}, 
    	{send_timeout, proplists:get_value(tcp_send_timeout, State#transstate.options, 30000)},
    	{send_timeout_close, true},
    	{keepalive, true},
    	{delay_send, false},
    	{nodelay, true},
        {packet, raw},
    	{reuseaddr, true}
    ]),

    NewState = reset_request(State#transstate{socket = Socket}),
    {noreply, NewState, NewState#transstate.inactive_timeout};

% Timeout waiting for client
handle_info(timeout, State) ->
	{stop, normal, State};

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
	{ok, State, State#transstate.inactive_timeout}.


%% ===================================================================
%% Support functions
%% ===================================================================

reset_request(State) ->
	State#transstate{procstate = ready, buffer = <<>>}.
