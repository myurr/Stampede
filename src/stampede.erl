-module(stampede).

%% Application helper functions
-export([start/0, listen/4, nodes/1]).

%% ===================================================================
%% Application helper functions
%% ===================================================================

start() ->
	ok = application:start(stampede),
	ok = bootstrap().


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


%% ===================================================================
%% Bootstrap stampede as a stand alone server
%% ===================================================================

bootstrap() ->
	SiteRoutes = [
		{set_path, <<"/var/www/ecademy2/">>},
		{site, ecademy}
	],

	EcRoutes = [
		{method, 'GET', [
			{path, <<"colin/web">>},
			{static_dir, <<"index.html">>, []}
		]}
	],

	stampede:nodes([]),
	ok = stampede_site:create(ecademy, EcRoutes, []),
	stampede:listen([{port, 8000}], SiteRoutes, stampede_site:list(), [{idle_workers, 20}]).
	ok.
