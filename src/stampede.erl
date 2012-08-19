-module(stampede).

%% Application helper functions
-export([start/0, listen/4, nodes/1]).

%% ===================================================================
%% Application helper functions
%% ===================================================================

start() ->
	ok = application:start(stampede).


%% ===================================================================
%% Listener helper functions
%% ===================================================================

listen(SocketDet, RoutingRules, SiteDefinitions, Options) ->
	case st_socket:listen(SocketDet) of
		{ok, ServerSocket} ->
			Id = proplists:get_value(id, Options, ServerSocket),
			stampede_sup:start_listener(Id, ServerSocket, RoutingRules, SiteDefinitions, Options);
		Err ->
			Err
	end.


%% ===================================================================
%% Initialise stampede, connect it to other nodes
%% ===================================================================

nodes(_NodeList) ->
	ok = stampede_site:init(),
	ok.



