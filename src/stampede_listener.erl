-module(stampede_listener).

-behaviour(gen_server).

% API
-export([start_link/3]).

% Supervisor callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(state, {server_socket = undefined, routing_rules = [], worker_count = 0, 
					max_workers = infinity, min_workers = 100}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServerSocket, RoutingRules, Options) ->
	gen_server:start_link(?MODULE, [ServerSocket, RoutingRules, Options], []).


%% ===================================================================
%% Gen_server callbacks
%% ===================================================================


%% =========================
%% Initialisation
%% =========================

init([ServerSocket, RoutingRules, Options]) ->
	io:format("Connection ~s initialised.~n", [st_socket:name(ServerSocket)]),
	State = #state{server_socket = ServerSocket, routing_rules = RoutingRules, worker_count = 0,
					max_workers = proplists:get_value(max_connections, Options, infinity),
					min_workers = proplists:get_value(idle_pool, Options, 100)},

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

handle_cast(Request, State) ->
	io:format("Unexpected cast to stampede_listener: ~p, ~p~n", [Request, State]),
	{noreply, State}.


%% =========================
%% Handle Info
%% =========================

% Initialisation trigger...  create the initial worker pool
handle_info(timeout, State) ->
	[ io:format("~p~n", [N]) || N <- lists:seq(1, State#state.min_workers) ],
	{noreply, State};

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

