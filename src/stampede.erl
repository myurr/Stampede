-module(stampede).

%% Application helper functions
-export([start/0]).
-export([listen/3]).

%% ===================================================================
%% Application helper functions
%% ===================================================================

start() ->
	application:start(stampede).



%% ===================================================================
%% Listener helper functions
%% ===================================================================

listen(SocketDet, RoutingRules, Options) ->
	case st_socket:listen(SocketDet) of
		{ok, ServerSocket} ->
			Id = proplists:get_value(id, Options, ServerSocket),
			stampede_sup:start_listener(Id, ServerSocket, RoutingRules, Options);
		Err ->
			Err
	end.
