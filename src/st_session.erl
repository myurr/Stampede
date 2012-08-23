-module(st_session).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API calls
-export([init_table/1, create/4, load/2, get_response_headers/1, expires/2, save/2,
			id/1]).

-record(ses_state, {next_tick = 0, table_names = []}).

-record(st_session, {id = undefined, cookie_name = <<"ses">>, expires = 0, uid = undefined, login_state = undefined, permissions = [],
						data = [], updated = false}).


-define(TICK_FREQUENCY, 60).

%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	State = #ses_state{},
	{ok, State, 0}.

handle_call(Request, From, State) ->
	io:format("Unexpected call to st_session: ~p, ~p, ~p~n", [Request, From, State]),
	{reply, ok, State, tick_timeout(State)}.

handle_cast({add_session_table, TableName}, State) ->
	NewState = State#ses_state{table_names = [TableName | State#ses_state.table_names]},
	{noreply, NewState, tick_timeout(NewState)};
handle_cast(Request, State) ->
	io:format("Unexpected cast to stampede_transport: ~p, ~p~n", [Request, State]),
	{noreply, State, tick_timeout(State)}.

handle_info(timeout, State) ->
	TickTimeout = tick(State),
	NewState = State#ses_state{next_tick = stutil:timestamp() + TickTimeout},
    {noreply, NewState, tick_timeout(NewState)};

handle_info(Msg, State) ->
	io:format("Unexpected info to st_session: ~p, ~p~n", [Msg, State]),
	{noreply, State, tick_timeout(State)}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State, tick_timeout(State)}.

%% ===================================================================
%% Server utility functions and API
%% ===================================================================

add_session_table(TableName) ->
	gen_server:cast(?MODULE, {add_session_table, TableName}).

tick(State) ->
	tidy_session_tables(State#ses_state.table_names),
	?TICK_FREQUENCY.

tick_timeout(State) ->
	TS = stutil:timestamp(),
	if State#ses_state.next_tick > TS -> (State#ses_state.next_tick - TS) * 1000;
	true -> 0
	end.

tidy_session_tables([TableName | Rest]) ->
	io:format("Deleting stale sessions from table ~p~n", [TableName]),

	TS = stutil:timestamp(),
	F = fun() ->
		qlc:eval(qlc:q(
			[ Id 
				|| #st_session{id=Id, expires=Expires} <- mnesia:table(TableName),
				Expires =< TS
			]))
	end,
	Delete = mnesia:activity(transaction, F),
	[ mnesia:async_dirty(fun() -> mnesia:delete(TableName, Id, write) end, [])|| Id <- Delete],
	io:format("Deleted sessions ~p~nDeleted in ~p second(s).~n", [Delete, stutil:timestamp() - TS]),
	tidy_session_tables(Rest);
tidy_session_tables([]) ->
	ok.


%% ===================================================================
%% Initialise the database
%% ===================================================================

init_table(SessionTableName) ->
	{atomic, ok} = mnesia:create_table(SessionTableName, [
									{attributes, record_info(fields, st_session)},
									{type, set}, 
									{index, [#st_session.expires]}, 
									{record_name, st_session}
								]),
	add_session_table(SessionTableName),
	io:format("Table created ~p!~n", [SessionTableName]),
	ok.


%% ===================================================================
%% Create a new session
%% ===================================================================

create(Site, CookieName, KeyLength, Timeout) ->
	SessionTable = stampede_site:session_table(Site),
	NewId = stutil:random_string(KeyLength),
	F = fun() ->
		case mnesia:read(SessionTable, NewId) of
			[] ->
				Session = #st_session{id = NewId, cookie_name = CookieName, expires = stutil:timestamp() + Timeout},
				mnesia:write(SessionTable, Session, write),
				Session;
			_ ->
				clash
		end
	end,
	case mnesia:activity(transaction, F) of
		clash ->
			create(Site, CookieName, KeyLength, Timeout);
		Ret ->
			Ret
	end.



%% ===================================================================
%% Look up a session
%% ===================================================================

load(_Site, undefined) ->
	undefined;
load(Site, Id) ->
	F = fun() ->
		case mnesia:read(stampede_site:session_table(Site), Id) of
			[] ->
				undefined;
			[Session] ->
				TS = stutil:timestamp(),
				if Session#st_session.expires < TS ->
					undefined;
				true ->
					Session#st_session{updated = false}
				end
		end
	end,
	mnesia:activity(transaction, F).


%% ===================================================================
%% Save the session
%% ===================================================================

save(_Site, Session) when Session#st_session.updated == false ->
	Session;
save(Site, Session) ->
	SessionTable = stampede_site:session_table(Site),
	SaveSession = Session#st_session{updated = false},
	F = fun() ->
		mnesia:write(SessionTable, SaveSession, write),
		SaveSession
	end,
	mnesia:activity(transaction, F).


%% ===================================================================
%% Output HTTP response headers
%% ===================================================================

get_response_headers(Session) ->
	case Session#st_session.id of
		undefined ->
			[];
		_ ->
			CookieVal = <<(Session#st_session.cookie_name)/binary, $=, (Session#st_session.id)/binary, "; HttpOnly">>,
			[{<<"Set-Cookie">>, CookieVal}]
	end.


%% ===================================================================
%% Update when the session expires
%% ===================================================================

expires(Session, Timeout) ->
	NewExpires = stutil:timestamp() + Timeout,
	if NewExpires == Session#st_session.expires ->
			Session;
		true -> 
			Session#st_session{expires = NewExpires, updated = true}
	end.



%% ===================================================================
%% Accessors
%% ===================================================================

id(Session) ->
	Session#st_session.id.
