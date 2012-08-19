
-module(stampede_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_listener/5]).

%% Supervisor callbacks
-export([init/1]).

%% Constants
-define(SHUTDOWN_TIME, 10000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_listener(ChildId, Socket, RoutingRules, SiteDefinitions, Options) ->
	ChildCmd = {stampede_listener, start_link, [Socket, RoutingRules, SiteDefinitions, Options]},
	ChildSpec = {ChildId, ChildCmd, permanent, ?SHUTDOWN_TIME, supervisor, [stampede_listener]},
	io:format("Starting child...~n"),
	supervisor:start_child(?MODULE, ChildSpec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 1, 10}, []} }.


%% ===================================================================
%% Internal functions
%% ===================================================================

