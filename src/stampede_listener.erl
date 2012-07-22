-module(stampede_listener).

-behaviour(supervisor).

% API
-export([start_link/3]).

% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServerSocket, RoutingRules, Options) ->
    supervisor:start_link(?MODULE, [ServerSocket, RoutingRules, Options]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

