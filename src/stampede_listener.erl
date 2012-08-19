-module(stampede_listener).

-behaviour(gen_server).

% API
-export([start_link/4, connection_accepted/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(state, {server_socket = undefined, routing_rules = [], worker_count = 0, 
					max_workers = infinity, min_workers = 1, worker_options = [],
					site_definitions = []}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServerSocket, RoutingRules, SiteDefinitions, Options) ->
	gen_server:start_link(?MODULE, [ServerSocket, RoutingRules, SiteDefinitions, Options], []).

connection_accepted(ParentPid) ->
	gen_server:cast(ParentPid, connection_accepted).


%% ===================================================================
%% Gen_server callbacks
%% ===================================================================


%% =========================
%% Initialisation
%% =========================

init([ServerSocket, RoutingRules, SiteDefinitions, Options]) ->
	io:format("Connection ~s initialised.~n", [st_socket:name(ServerSocket)]),
	State = #state{server_socket = ServerSocket, routing_rules = RoutingRules, worker_count = 0,
					site_definitions = SiteDefinitions,
					worker_options = proplists:get_value(workers, Options, []),
					max_workers = proplists:get_value(max_connections, Options, infinity),
					min_workers = proplists:get_value(idle_workers, Options, 10)},

	% We want to receive a message when a child exits, so we can reduce the worker count
	process_flag(trap_exit, true),
	
	{ok, State, 0}.


%% =========================
%% Handle Call
%% =========================

handle_call(Request, From, State) ->
	io:format("Unexpected call to stampede_listener: ~p, ~p, ~p~n", [Request, From, State]),
	{reply, ok, State}.


%% =========================
%% Handle Cast
%% =========================

handle_cast(connection_accepted, State) ->
	NewState = start_child(State),
	{noreply, NewState};

handle_cast(Request, State) ->
	io:format("Unexpected cast to stampede_listener: ~p, ~p~n", [Request, State]),
	{noreply, State}.


%% =========================
%% Handle Info
%% =========================

% Initialisation trigger...  create the initial worker pool
handle_info(timeout, State) ->
	start_children(State, State#state.min_workers),
	{noreply, State};

% Exit signal
handle_info({'EXIT', _PID, _Reason}, State) ->
	io:format("Exit signal received.~n"),
	{noreply, start_child(State)};

% Unknown signal
handle_info(Msg, State) ->
	io:format("Unexpected info to stampede_listener: ~p, ~p~n", [Msg, State]),
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

start_children(State, NumToStart) when NumToStart > 0 ->
	NewState = start_child(State),
	start_children(NewState, NumToStart - 1);
start_children(State, _) ->
	State.

start_child(State) ->
	stampede_transport:start_link(self(), State#state.server_socket, State#state.routing_rules,
											State#state.site_definitions, State#state.worker_options),
	State#state{worker_count = State#state.worker_count + 1}.

