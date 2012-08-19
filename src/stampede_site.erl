-module(stampede_site).

-export([init/0, create/3, list/0, lookup/2]).
-export([routes/1, session_table/1, config/1, config/2, config/3]).

-record(stampede_site, {id, session_table, cache_sup, cache_table, cache_dir, notify_pids, routing, config}).

%% ===================================================================
%% Initialise the database
%% ===================================================================

init() ->
	mnesia:create_table(stampede_site, [{attributes, record_info(fields, stampede_site)},
										{type, set}]),
	io:format("Table created!~n"),
	ok.


%% ===================================================================
%% Create a new site
%% ===================================================================

create(Name, Routing, Options) ->
	io:format("Called with name ~p~n", [Name]),
	SessionTable = proplists:get_value(session_table, Options, list_to_atom(atom_to_list(Name) ++ "_session")),
	CacheTable = proplists:get_value(cache_table, Options, list_to_atom(atom_to_list(Name) ++ "_cache")),
	CacheSuperVisor = proplists:get_value(cache_sup, Options, list_to_atom(atom_to_list(Name) ++ "_cache_sup")),
	CacheDir = proplists:get_value(cache_dir, Options, <<"/tmp/stampede/", (atom_to_binary(Name, latin1))/binary>>),
	Config = proplists:get_value(config, Options, []),
	Site = #stampede_site{id = Name, session_table = SessionTable, cache_table = CacheTable,
							cache_sup = CacheSuperVisor, notify_pids = [],
							cache_dir = proplists:get_value(cache_dir, Options, CacheDir),
							routing = Routing, config = Config},
	F = fun() ->
			io:format("Mnesia: ~p~n", [mnesia:read(stampede_site, Name)]),
			case mnesia:read(stampede_site, Name) =:= [] of
				true ->
					mnesia:write(Site),
					ok;
				false ->
					{error, site_name_clash}
			end
	end,
	case mnesia:activity(transaction, F) of
		ok ->
			st_session:init(SessionTable),
			ok;
		Err ->
			Err
	end.


%% ===================================================================
%% Export all sites
%% ===================================================================

list() ->
	F = fun() ->
		[ Site || SiteId <- mnesia:all_keys(stampede_site), Site <- mnesia:read(stampede_site, SiteId)]
	end,
	mnesia:activity(transaction, F).


%% ===================================================================
%% Look up a site from a list of sites
%% ===================================================================

lookup([Site | List], SiteId) ->
	case Site#stampede_site.id of
		SiteId -> Site;
		_ -> lookup(List, SiteId)
	end;
lookup([], _Id) ->
	undefined.


%% ===================================================================
%% Accessors
%% ===================================================================

routes(Site) ->
	Site#stampede_site.routing.

session_table(Site) ->
	Site#stampede_site.session_table.

config(Site) ->
	Site#stampede_site.config.

config(Site, Key) ->
	proplists:get_value(Key, Site#stampede_site.config).

config(Site, Key, Default) ->
	proplists:get_value(Key, Site#stampede_site.config, Default).
