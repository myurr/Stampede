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
		{set_path, <<"/var/www/ammpo/">>},
		{site, ecademy}
	],

	EcRoutes = [
		{url, <<"/ws">>, [
			{jquery_socket, all, [{stmq_subscribe, [test]}, {stmq_msg, auto_forward}], [
				{init, fun tweet_http:jq_tweet_init/1},
				{rx, fun tweet_http:jq_tweet_receive/2}
			]}
		]},

		{method, 'GET', [
			{symfony_root, <<"colin/web">>, [{connect, [{"localhost", 9000}]}, {default_path, <<"app.php">>}]}
		]}
	],

	stampede:nodes([]),
	ok = stampede_site:create(ecademy, EcRoutes, []),
	stampede:listen([{port, 8000}], SiteRoutes, stampede_site:list(), [{idle_workers, 20}]),
	stampede:cmd_listen([{port, 8001}], stampede_site:list(), []),
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
