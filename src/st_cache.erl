-module(st_cache).

% gen_server callbacks
-export([async_generate_content/6, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([status_code/1, expires/1, cache_file/1, etag/1, error_detail/1, content_type/1]).

% Exported API
-export([init_cache/2, send_resource/5]).

-define(CACHE_GENERATOR_LINGER_TIME, 10000).

%% ===================================================================
%% Definitions
%% ===================================================================

-record(cache_state, {status = init, ci = undefined, site, urn, request, rst, subrules, options}).

-record(st_cache, {urn = <<>>, expires = 0, cache_filename = undefined, generator_pid = undefined, etag = undefined,
					http_status = 404, content_type = undefined, error_detail = undefined, nodes = []}).



%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

async_generate_content(Site, Urn, Request, Rst, SubRules, Options) ->
	{ok, Pid} = gen_server:start(?MODULE, [Site, Urn, Request, Rst, SubRules, Options], []),
	Pid.

init([Site, Urn, Request, Rst, SubRules, Options]) ->
	State = #cache_state{site = Site, urn = Urn, request = Request, rst = Rst, subrules = SubRules, options = Options},
	{ok, State, 0}.

handle_call(Request, From, State) ->
	io:format("Unexpected call to st_cache: ~p, ~p, ~p~n", [Request, From, State]),
	{reply, ok, State, ?CACHE_GENERATOR_LINGER_TIME}.

handle_cast({send_content, ReplyPid}, State) ->
    ReplyPid ! {cache_content, State#cache_state.ci},
    {noreply, State, ?CACHE_GENERATOR_LINGER_TIME};
handle_cast(Request, State) ->
	io:format("Unexpected cast to st_cache: ~p, ~p~n", [Request, State]),
	{noreply, State, ?CACHE_GENERATOR_LINGER_TIME}.

handle_info(timeout, State) when State#cache_state.status == init ->
	case st_routing:rule(State#cache_state.rst, State#cache_state.subrules, State#cache_state.request) of
	 	{send, Response} ->
	 		case st_response:body_type(Response) of
	 			stream ->
                    NewCI = #st_cache{urn = State#cache_state.urn,
                                        expires = stutil:timestamp() + 60,
                                        http_status = 500,
                                        error_detail = <<"Streamed responses cannot be cached.">>},
                    NewState = State#cache_state{status = linger, ci = NewCI},
                    mnesia:activity(transaction, fun() ->
                            mnesia:write(stampede_site:cache_table(NewState#cache_state.site), NewCI, write)
                        end),
	 				{noreply, NewState, ?CACHE_GENERATOR_LINGER_TIME};
	 			_ ->
			 		NewCI = #st_cache{urn = State#cache_state.urn,
			 							expires = calc_expires(State#cache_state.options),
			 							cache_filename = calc_filename(State#cache_state.site, State#cache_state.urn),
			 							etag = st_response:calc_etag(Response),
                                        content_type = st_response:get_header(Response, <<"Content-Type">>, undefined),
			 							http_status = 200},
			 		ok = st_response:save_to_file(Response, NewCI#st_cache.cache_filename),
			 		NewState = State#cache_state{status = linger, ci = NewCI},
                    mnesia:activity(transaction, fun() ->
                            mnesia:write(stampede_site:cache_table(NewState#cache_state.site), NewCI, write)
                        end),
			 	   {noreply, NewState, ?CACHE_GENERATOR_LINGER_TIME}
			end;
        {cache_hit, _Request, CI} ->
            NewCI = CI#st_cache{urn = State#cache_state.urn,
                                expires = calc_expires(State#cache_state.options),
                                cache_filename = calc_filename(State#cache_state.site, State#cache_state.urn),
                                http_status = 200},
            {ok, _Bytes} = file:copy(CI#st_cache.cache_filename, NewCI#st_cache.cache_filename),
            NewState = State#cache_state{status = linger, ci = NewCI},
            mnesia:activity(transaction, fun() ->
                    mnesia:write(stampede_site:cache_table(NewState#cache_state.site), NewCI, write)
                end),
           {noreply, NewState, ?CACHE_GENERATOR_LINGER_TIME};
	 	{error, StatusCode, Detail} ->
            NewCI = #st_cache{urn = State#cache_state.urn,
                                expires = stutil:timestamp() + 60,
                                http_status = StatusCode,
                                error_detail = <<"Error generating cached content: ", Detail/binary>>},
            NewState = State#cache_state{status = linger, ci = NewCI},
            mnesia:activity(transaction, fun() ->
                    mnesia:write(stampede_site:cache_table(NewState#cache_state.site), NewCI, write)
                end),
	 	   {noreply, NewState, ?CACHE_GENERATOR_LINGER_TIME};
	 	{cache_async, _Request, _Pid} ->
            NewCI = #st_cache{urn = State#cache_state.urn,
                                expires = stutil:timestamp() + 60,
                                http_status = 500,
                                error_detail = <<"Trying to cache not yet cached content.">>},
            NewState = State#cache_state{status = linger, ci = NewCI},
            mnesia:activity(transaction, fun() ->
                    mnesia:write(stampede_site:cache_table(NewState#cache_state.site), NewCI, write)
                end),
	 	   {noreply, NewState, ?CACHE_GENERATOR_LINGER_TIME}
	end;

handle_info(timeout, State) ->
	{stop, normal, State};

handle_info(Msg, State) ->
	io:format("Unexpected info to st_session: ~p, ~p~n", [Msg, State]),
	{noreply, State, ?CACHE_GENERATOR_LINGER_TIME}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State, ?CACHE_GENERATOR_LINGER_TIME}.


%% ===================================================================
%% API functions
%% ===================================================================

init_cache(CacheTableName, CacheDir) ->
	{atomic, ok} = mnesia:create_table(CacheTableName, [
									{attributes, record_info(fields, st_cache)},
									{type, set}, 
									{index, [#st_cache.expires]}, 
									{record_name, st_cache}
								]),
	io:format("Creating cache dir ~p~n", [CacheDir]),
	ok = filelib:ensure_dir(<<CacheDir/binary, $/>>),
	ok.

send_resource(CacheUrn, Request, Rst, SubRules, Options) ->
	Site = st_request:site(Request),
	case quick_lookup(Site, CacheUrn) of
		{ok, CI} ->
			{cache_hit, Request, CI};
		not_found ->
			case full_lookup(Site, CacheUrn, Request, Rst, SubRules, Options) of
				{ok, CI} ->
                    {cache_hit, Request, CI};
				{cache_fetch, Pid} ->
					request_content(Pid),
					{cache_async, Request, Pid}
			end
	end.

%% ===================================================================
%% Accessors
%% ===================================================================

status_code(CI) ->
    CI#st_cache.http_status.

expires(CI) ->
    CI#st_cache.expires.

cache_file(CI) ->
    CI#st_cache.cache_filename.

etag(CI) ->
    CI#st_cache.etag.

error_detail(CI) ->
    CI#st_cache.error_detail.

content_type(CI) ->
    CI#st_cache.content_type.


%% ===================================================================
%% Support functions
%% ===================================================================

quick_lookup(Site, Urn) ->
	TS = stutil:timestamp(),
	F = fun() ->
		case mnesia:read(stampede_site:cache_table(Site), Urn) of
			[CI] when CI#st_cache.expires == infinity; CI#st_cache.expires > TS -> {ok, CI};
			_ -> not_found
		end
	end,
	mnesia:async_dirty(F).

full_lookup(Site, Urn, Request, Rst, SubRules, Options) ->
	TS = stutil:timestamp(),
	F = fun() ->
		case mnesia:read(stampede_site:cache_table(Site), Urn, write) of
			[CI] when CI#st_cache.expires == infinity; CI#st_cache.expires > TS -> % We have a valid record
				{ok, CI};
			[CI] when CI#st_cache.generator_pid /= undefined -> % We have a record that is being generated
				{cache_fetch, CI#st_cache.generator_pid};
			_ -> % Record expired or does not exist
				Pid = async_generate_content(Site, Urn, Request, Rst, SubRules, Options),
				NewCI = #st_cache{urn = Urn, expires = 0, generator_pid = Pid},
				mnesia:write(stampede_site:cache_table(Site), NewCI, write),
				{cache_fetch, Pid}
		end
	end,

	mnesia:activity(transaction, F).

calc_expires(Options) ->
	Delta = case proplists:get_value(expires, Options, 1) of
		Int when is_integer(Int) ->
			Int
	end,
	stutil:timestamp() + Delta.

calc_filename(Site, Urn) ->
	BaseDir = stampede_site:cache_dir(Site),
	Hash = base64:encode(crypto:sha(<<(stampede_site:name_str(Site))/binary, $:, Urn/binary>>)),
    FinalHash = binary:replace(Hash, <<$/>>, <<$_>>, [global]),
	<<BaseDir/binary, (hash_to_filename(FinalHash, 4, <<>>))/binary, ".stc">>.

hash_to_filename(<<Chr1, Chr2, Rest/binary>>, Counter, Acc) when Counter > 0 ->
	hash_to_filename(Rest, Counter - 1, <<Acc/binary, $/, Chr1, Chr2>>);
hash_to_filename(Rest, 0, Acc) ->
	<<Acc/binary, $/, Rest/binary>>.

request_content(Pid) ->
	gen_server:cast(Pid, {send_content, self()}).

