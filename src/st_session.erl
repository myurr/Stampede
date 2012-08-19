-module(st_session).

-export([init/1, create/4, load/2, get_response_headers/1, expires/2, save/2]).

-record(st_session, {id = undefined, cookie_name = <<"ses">>, expires = 0, uid = undefined, login_state = undefined, permissions = [],
						data = [], updated = false}).

%% ===================================================================
%% Initialise the database
%% ===================================================================

init(SessionTableName) ->
	{atomic, ok} = mnesia:create_table(SessionTableName, [
									{attributes, record_info(fields, st_session)},
									{type, set}, 
									{index, [#st_session.expires]}, 
									{record_name, st_session}
								]),
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
				Session#st_session{updated = false}
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
