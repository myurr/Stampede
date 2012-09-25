-module(st_request).

% Exported API
-export([new/4, terminate/1, header/3, end_headers/1, execute/3, error_request/4,
		http_version/1, method/1, url/1, host/1, hostname/1, arg/2, arg/3, keepalive/1, content_type/1, content_length/1,
		content_read/1, content_unread/1, content_read/2, post_data/1, post_arg/2, post_arg/3, process_post_data/2,
		if_modified_since/1, lookup_session/2, session/1, save_session/1, site/1, site/2, cookie/2, cookie/3,
		discard_post_data/2, url_add_args/2, url_arg/2, url_arg/3, if_none_match/1, query_string/1,
		get_header/2, get_header/3
		]).

%% ===================================================================
%% Definitions
%% ===================================================================

-define(DEFAULT_KEEPALIVE, 30).

-record(st_request, {socket = undefined, error = undefined, method, url, full_url, args = [], post_args = [], url_args = [],
						http_version = {1, 1}, raw_post_data = <<>>,
						headers = [], content_length = 0, content_read = 0, content_type = undefined, host = undefined, 
						keepalive = false, site = undefined, cookies = [], if_modified_since = undefined, if_none_match = undefined,
						session = undefined, prev_requests = []}).


%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Create a new request
%% ====================

new(Socket, Method, Path, Version) ->
	{ok, Url, Args} = decode_url(Path),
	% io:format("Method ~p, URL ~p, Args ~p, Version ~p~n", [Method, Url, Args, Version]),
	{ok, #st_request{socket = Socket, method = Method, url = Url, full_url = Path, args = Args, http_version = Version,
					keepalive = if Version == {1,1} -> ?DEFAULT_KEEPALIVE; true -> false end}}.


%% ====================
%% Terminate a request
%% ====================

terminate(Request) ->
	{ok, Request}.


%% ====================
%% Set a header
%% ====================

header(Request, 'Host', Value) ->
	case binary:split(Value, <<$:>>) of
		[Host, Port] ->
			{ok, Request#st_request{host = {Host, Port}}};
		[Host] ->
			{ok, Request#st_request{host = {Host, st_socket:port(Request#st_request.socket)}}}
	end;

header(Request, 'Content-Length', Value) ->
	{ok, Request#st_request{content_length = stutil:to_integer(Value)}};

header(Request, 'Content-Type', Value) ->
	{ok, Request#st_request{content_type = Value}};

header(Request, 'Connection', Value) ->
	header_connection(Request, [ stutil:trim_str(stutil:bstr_to_lower(C)) || C <- binary:split(Value, <<",">>, [global]) ]);

header(Request, 'If-Modified-Since', Value) ->
	{ok, Request#st_request{if_modified_since = httpd_util:convert_request_date(binary_to_list(Value))}};

header(Request, 'If-None-Match', Value) ->
	{ok, Request#st_request{if_none_match = Value}};

header(Request, 'Cookie', Value) ->
	{ok, Request#st_request{cookies = decode_cookies(binary:split(Value, <<$;>>, [global]), Request#st_request.cookies)}};

header(Request, Key, Value) when is_atom(Key) ->
	{ok, Request#st_request{headers = [{Key, Value} | Request#st_request.headers]}};
header(Request, Key, Value) ->
	{ok, Request#st_request{headers = [{stutil:bstr_to_lower(Key), Value} | Request#st_request.headers]}}.


header_connection(Request, [Value | Rest]) ->
	case Value of
		<<"keep-alive">> ->
			header_connection(Request#st_request{keepalive = ?DEFAULT_KEEPALIVE}, Rest);
		<<"close">> ->
			header_connection(Request#st_request{keepalive = false}, Rest);
		_ ->
			header_connection(Request#st_request{headers = [{'Connection', Value} | Request#st_request.headers]}, Rest)
	end;
header_connection(Request, []) ->
	{ok, Request}.


%% ====================
%% End of the headers
%% ====================

end_headers(Request) ->
	% io:format("Request:~n~p~n~n", [Request]),
	{ok, Request}.


%% ====================
%% Execute a request
%% ====================

execute(Request, RoutingRules, SiteDefinitions) ->
	st_routing:route(Request, RoutingRules, SiteDefinitions).



%% ====================
%% Create a new error request
%% ====================

error_request(Request, StatusCode, Error, Detail) ->
	Request#st_request{method = 'ERROR', url = stutil:to_binary(StatusCode), 
						args = [{<<"error">>, stutil:to_binary(Error)}, {<<"detail">>, stutil:to_binary(Detail)}],
						headers = [],
						prev_requests = [Request | Request#st_request.prev_requests]}.


%% ====================
%% Session Management
%% ====================

lookup_session(Request, _Options) when Request#st_request.session /= undefined ->
	% io:format("Session already loaded.~n"),
	{ok, Request};
lookup_session(Request, Options) ->
	SessionCookie = proplists:get_value(session_cookie_name, Options,
						proplists:get_value(session_cookie_name, stampede_site:config(site(Request)), <<"sid">>)),
	case st_session:load(site(Request), cookie(Request, SessionCookie)) of
		undefined ->
			NewSession = st_session:create(site(Request), SessionCookie,
							proplists:get_value(session_cookie_length, Options, stampede_site:config(site(Request), session_cookie_length, 24)), 
							proplists:get_value(session_timeout_unused, Options, stampede_site:config(site(Request), session_timeout_unused, 300))
							),
			% io:format("New session created ~p~n", [NewSession]),
			{ok, Request#st_request{session = NewSession}};
		Session ->
			FinalSession = case proplists:get_value(reset_timeout, Options, true) of
				true -> 
					Timeout = proplists:get_value(session_timeout_used, Options, stampede_site:config(site(Request), session_timeout_used, 1200)),
					st_session:expires(Session, Timeout);
				false ->
					Session
			end,
			% io:format("Session loaded ~p~n", [FinalSession]),
			{ok, Request#st_request{session = FinalSession}}
	end.


save_session(Request) when Request#st_request.session == undefined ->
	% io:format("No session to save...~n"),
	Request;
save_session(Request) ->
	% io:format("Saving session~n"),
	Request#st_request{session = st_session:save(site(Request), session(Request))}.


%% ====================
%% Tidy up after ourselves
%% ====================

discard_post_data(Request, Timeout) ->
	Len = content_unread(Request),
	if Len > 0 -> st_socket:recv(Request#st_request.socket, Len, Timeout);
		true -> ok
	end,
	Request#st_request{content_read = Request#st_request.content_read + Len}.


%% ====================
%% Accessors
%% ====================

http_version(Request) ->
	Request#st_request.http_version.

method(Request) ->
	Request#st_request.method.

url(Request) ->
	Request#st_request.url.

host(Request) ->
	Request#st_request.host.

hostname(Request) ->
	case Request#st_request.host of
		{Host, _Port} -> Host;
		_ -> st_socket:name(Request#st_request.socket)
	end.

arg(Request, Key) ->
	proplists:get_value(Key, Request#st_request.args).

arg(Request, Key, Default) ->
	proplists:get_value(Key, Request#st_request.args, Default).

post_data(Request) ->
	Request#st_request.raw_post_data.

post_arg(Request, Key) ->
	proplists:get_value(Key, Request#st_request.post_args).

post_arg(Request, Key, Default) ->
	proplists:get_value(Key, Request#st_request.post_args, Default).

url_add_args(Request, ArgsList) ->
	Request#st_request{url_args = ArgsList ++ Request#st_request.url_args}.

url_arg(Request, Key) ->
	proplists:get_value(Key, Request#st_request.url_args).

url_arg(Request, Key, Default) ->
	proplists:get_value(Key, Request#st_request.url_args, Default).


keepalive(Request) ->
	% false.
	Request#st_request.keepalive.

if_modified_since(Request) ->
	Request#st_request.if_modified_since.

if_none_match(Request) ->
	Request#st_request.if_none_match.

session(Request) ->
	Request#st_request.session.

site(Request) ->
	Request#st_request.site.

site(Request, SetSite) ->
	Request#st_request{site = SetSite}.

cookie(Request, Key) ->
	proplists:get_value(Key, Request#st_request.cookies).

cookie(Request, Key, Default) ->
	proplists:get_value(Key, Request#st_request.cookies, Default).

content_type(Request) ->
	Request#st_request.content_type.

content_length(Request) ->
	Request#st_request.content_length.

content_read(Request, SetLen) ->
	Request#st_request{content_read = SetLen}.

content_read(Request) ->
	Request#st_request.content_read.

content_unread(Request) ->
	Request#st_request.content_length - Request#st_request.content_read.

query_string(Request) ->
	Request#st_request.full_url.

get_header(Request, Key) ->
	proplists:get_value(Key, Request#st_request.headers).

get_header(Request, Key, Default) ->
	proplists:get_value(Key, Request#st_request.headers, Default).


%% ===================================================================
%% Handle post data
%% ===================================================================

process_post_data(Request, Timeout) when Request#st_request.content_type == <<"application/x-www-form-urlencoded">> ->
	Len = content_unread(Request),
	{ok, Data} = st_socket:recv(Request#st_request.socket, Len, Timeout),
	PostArgs = decode_url_args(binary:split(Data, <<$&>>, [global]), []),
	Request#st_request{post_args = PostArgs, raw_post_data = Data, content_read = Request#st_request.content_read + Len};

process_post_data(Request, Timeout) ->
	Len = content_unread(Request),
	{ok, Data} = st_socket:recv(Request#st_request.socket, Len, Timeout),
	Request#st_request{raw_post_data = Data, content_read = Request#st_request.content_read + Len}.


%% ===================================================================
%% Internal functions
%% ===================================================================

decode_url(Path) ->
	case binary:split(Path, <<$?>>) of
		[Url] ->
			{ok, Url, []};
		[Url, Args] ->
			{ok, Url, decode_url_args(binary:split(Args, <<$&>>, [global]), [])}
	end.

decode_url_args([Arg | Rest], ArgList) ->
	case binary:split(Arg, <<$=>>) of
		[Key] ->
			decode_url_args(Rest, [{stutil:urldecode(Key), <<>>} | ArgList]);
		[Key, Value] ->
			decode_url_args(Rest, [{stutil:urldecode(Key), stutil:urldecode(Value)} | ArgList])
	end;
decode_url_args([], ArgList) ->
	ArgList.

decode_cookies([Cookie | Rest], Cookies) ->
	case binary:split(Cookie, <<$=>>) of
		[Key, Value] ->
			% io:format("Received cookie ~p = ~p~n", [stutil:trim_str(Key), stutil:trim_str(Value)]),
			decode_cookies(Rest, [{stutil:trim_str(Key), stutil:trim_str(Value)} | Cookies]);
		BadVal ->
			io:format("Skipping badly formatted cookie ~p~n", [BadVal]),
			decode_cookies(Rest, Cookies)
	end;
decode_cookies([], Cookies) ->
	Cookies.


