-module(stampede_config).

-behaviour(gen_server).

% API
-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(state, {config_file, options}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ConfigFile, Options) ->
	gen_server:start_link(?MODULE, [ConfigFile, Options], []).


%% ===================================================================
%% Gen_server callbacks
%% ===================================================================


%% =========================
%% Initialisation
%% =========================

init([ConfigFile, Options]) ->
	State = #state{config_file = ConfigFile, options = Options},

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
	% Bootstrap everything...
	SiteRoutes = [
		{set_path, <<"/var/www/ecademy2/">>},
		{site, ecademy}
	],

	EcRoutes = [
		{method, 'GET', [
			{path, <<"colin/web">>},
			{fcgi, <<"/var/www/ecademy2/colin/web/app_dev.php">>, [{connect, [{"localhost", 9000}]}]}
%			{static_dir, <<"index.html">>, []}
		]}
	],

	stampede:nodes([]),
	ok = stampede_site:create(ecademy, EcRoutes, []),
	stampede:listen([{port, 8000}], SiteRoutes, stampede_site:list(), [{idle_workers, 20}]),
	{noreply, State};

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
