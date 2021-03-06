-module(st_routing).

% Exported API
-export([route/3, rule/3]).

%% ===================================================================
%% Definitions
%% ===================================================================

-include_lib("kernel/include/file.hrl").

-record(rst, {site = undefined, site_definitions = [], path = undefined, base_path = undefined,
				routes = [],
				url_parts = [], browser_cache_for = undefined, session = undefined}).

%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Route a request
%% ====================

route(Request, Rules, SiteDefinitions) ->
	% io:format("Routing URL ~p~n", [st_request:url(Request)]),
	Rst = #rst{site_definitions = SiteDefinitions, url_parts = split_url(st_request:url(Request)), routes = Rules},
	rule(Rst, Rules, Request).


%% ===================================================================
%% Rule matching and execution functions
%% ===================================================================

% Match the request method
rule(Rst, [{method, Method, SubRules} | Rules], Request) ->
	RMethod = st_request:method(Request),
	if RMethod == Method ->
		rule(Rst, SubRules, Request);
	true ->
		rule(Rst, Rules, Request)
	end;

% Match a host entry
rule(Rst, [{host, Host, SubRules} | Rules], Request) ->
	% io:format("Compare ~p vs ~p~n", [st_request:hostname(Request), stutil:make_list(Host)]),
	HostMatch = lists:member(st_request:hostname(Request), stutil:make_list(Host)),
	if HostMatch == true ->
		rule(Rst, SubRules, Request);
	true ->
		rule(Rst, Rules, Request)
	end;

% Set the path
rule(Rst, [{set_path, Path} | Rules], Request) ->
	rule(set_path(Rst, Path), Rules, Request);

% Append to the path
rule(Rst, [{path, Path} | Rules], Request) ->
	rule(append_path(Rst, Path), Rules, Request);

% Match the content type
rule(Rst, [{content_type, MatchType, SubRules} | Rules], Request) ->
	CT = st_request:content_type(Request),
	if CT == MatchType ->
		rule(Rst, SubRules, Request);
	true ->
		rule(Rst, Rules, Request)
	end;

% Process post arguments
rule(Rst, [{post_args, MaxSize, Options} | Rules], Request) ->
	AllowSize = stutil:size_to_bytes(MaxSize),
	ContentLength = st_request:content_length(Request),
	ContentRead = st_request:content_read(Request),
	if AllowSize >= ContentLength, ContentRead == 0 ->
		NewRequest = st_request:process_post_data(Request, proplists:get_value(timeout, Options, 60) * 1000),
		rule(Rst, Rules, NewRequest);
	ContentRead > 0 ->
		{error, bad_request, <<"Posted content is longer than the maximum allowed.">>};
	true ->
		rule(Rst, Rules, Request)
	end;


% Match a URL
rule(Rst, [{url, MatchRule, SubRules} | Rules], Request) ->
	% io:format("Url match: ~p vs ~p = ~p~n", [Rst#rst.url_parts, split_url(MatchRule), 
												% url_match(Rst#rst.url_parts, split_url(MatchRule))]),
	case url_match(Rst#rst.url_parts, split_url(MatchRule), []) of
		{match, RemainingUrl, UrlArgs} ->
			rule(Rst#rst{url_parts = RemainingUrl}, SubRules, st_request:url_add_args(Request, UrlArgs));
		false ->
			rule(Rst, Rules, Request)
	end;

% Map a URL to a fixed file
rule(Rst, [{map_file, MatchRule, ServeFileName, Options} | Rules], Request) ->
	case url_match(Rst#rst.url_parts, split_url(MatchRule), []) of
		{match, [], _UrlArgs} ->
			% if Rst#rst.path == undefined -> io:format("Error: path has not been set.~n"); true -> ok end,
			FileName = <<(Rst#rst.path)/binary, $/, ServeFileName/binary>>,
			MimeType = proplists:get_value(mime_type, Options, st_mime_type:get_type(filename:extension(FileName))),
			case file_modified(FileName, Request) of
				true ->
					{ok, Response} = st_response:new(Request, ok, [{<<"Content-Type">>, MimeType}], {file, FileName}),
					{send, finalise_response(Response, Rst, Request)};
				false ->
					{ok, Response} = st_response:new(Request, not_modified, [{<<"Content-Type">>, MimeType}], <<>>),
					{send, finalise_response(Response, Rst, Request)}
			end;
		_ ->
			rule(Rst, Rules, Request)
	end;

% Hit the cache
rule(Rst, [{cache, CacheUrlDef, Options, SubRules} | _Rules], Request) ->
	CacheUrl = gen_url(CacheUrlDef, Rst, Request, <<>>),
	st_cache:send_resource(CacheUrl, Request, Rst, SubRules, Options);

% Match a static directory of content
rule(Rst, [{static_dir, DefaultFile, Options} | Rules], Request) ->
	BaseDir = Rst#rst.path,
	FileName = case Rst#rst.url_parts of
		[] -> <<BaseDir/binary, $/, DefaultFile/binary>>;
		[<<>>] -> <<BaseDir/binary, $/, DefaultFile/binary>>;
		Parts -> combine_url_path(Parts, BaseDir)
	end,
	case serve_static_file(FileName, Options) of
		true ->
			MimeType = proplists:get_value(mime_type, Options, st_mime_type:get_type(filename:extension(FileName))),
			case file_modified(FileName, Request) of
				true ->
					{ok, Response} = st_response:new(Request, ok, [{<<"Content-Type">>, MimeType}], {file, FileName}),
					{send, finalise_response(Response, Rst, Request)};
				false ->
					{ok, Response} = st_response:new(Request, not_modified, [{<<"Content-Type">>, MimeType}], <<>>),
					{send, finalise_response(Response, Rst, Request)}
			end;
		false ->
			rule(Rst, Rules, Request)
	end;

% Browser cache control
rule(Rst, [{http_expires, undefined} | Rules], Request) ->
	rule(Rst#rst{browser_cache_for = undefined}, Rules, Request);
rule(Rst, [{http_expires, Delta} | Rules], Request) ->
	rule(Rst#rst{browser_cache_for = Delta}, Rules, Request);

% Send some static content
rule(Rst, [{static, Content} | _Rules], Request) ->
	{ok, Response} = st_response:new(Request, ok, [], Content),
	{send, finalise_response(Response, Rst, Request)};


% We know which site we are now, replace the rules with the site's rules
rule(Rst, [{site, SiteName} | _Rules], Request) ->
	case stampede_site:lookup(Rst#rst.site_definitions, SiteName) of
		undefined ->
			{error, site_not_found, <<"Site definition ", (stutil:to_binary(SiteName))/binary, " not found.">>};
		Site ->
			Routes = stampede_site:routes(Site),
			rule(Rst#rst{site = Site, routes = Routes}, Routes, st_request:site(Request, Site))
	end;


% Session handling
rule(Rst, [{session, _Options} | _Rules], _Request) when Rst#rst.site =:= undefined ->
	{error, 500, <<"Session rule called before a site has been set.">>};
rule(Rst, [{session, Options} | Rules], Request) ->
	{ok, NewRequest} = st_request:lookup_session(Request, Options),
	rule(Rst, Rules, NewRequest);


% Dynamic erlang call
rule(Rst, [{erlang, CallDetails} | Rules], Request) ->
	case call_erlang(CallDetails, Rst, Request) of
		{reroute, Url} ->
			rule(Rst#rst{url_parts = split_url(Url)}, Rst#rst.routes, Request);
		{continue} ->
			rule(Rst, Rules, Request);
		{continue, NewRequest} ->
			rule(Rst, Rules, NewRequest);
		{continue, NewRequest, NewRst} ->
			rule(NewRst, Rules, NewRequest);
		{continue, NewRequest, NewRst, NewRules} ->
			rule(NewRst, NewRules, NewRequest);
		{send, Response} ->
			{send, finalise_response(Response, Rst, Request)};
		{raw_send, Response} ->
			{send, Response}
	end;

rule(OrigRst, [{symfony_root, Path, Options} | _Rules], Request) ->
	Rst = append_path(OrigRst, Path),

	ReqMethod = st_request:method(Request),
	if ReqMethod == 'POST' ->
			AllowSize = stutil:size_to_bytes(proplists:get_value(max_post_data, Options, {64, kb})),
			Timeout = proplists:get_value(timeout, Options, 60) * 1000,
			ContentLength = st_request:content_length(Request),
			ContentRead = st_request:content_read(Request),
			if AllowSize >= ContentLength, ContentRead == 0 ->
				NewRequest = st_request:process_post_data(Request, Timeout),
				process_symfony_request(Rst, Options, NewRequest, st_request:post_data(NewRequest));
			true ->
				{error, 400, <<"Post data too long.">>}
			end;
		true ->
			process_symfony_request(Rst, Options, Request, <<>>)
	end;


% Fast CGI request - {fcgi, <<"/www/sites/test/php/test.php">>, [{connect, [{"localhost", 9000}]}]}
rule(Rst, [{fcgi, Script, Options} | _Rules], Request) ->
	FCGI = st_fcgi:new(proplists:get_value(connect, Options, [])),
	% FCGI_Params = fcgi_params(FCGI, Rst, Script, Request, undefined, Options),
	FCGI_Params = fcgi_params(FCGI, Script, Rst, Request, [
			
		]),
	FCGI_End = st_fcgi:stdin_end(FCGI_Params),
	case st_fcgi:execute(Request, FCGI_End) of
		{ok, Response} ->
			{send, Response};
		{error, Reason} ->
			{error, 500, Reason}
	end;

rule(_Rst, [{jquery_socket, AllowOrigin, Options, CallBacks} | _Rules], Request) ->
	case st_request:method(Request) of
		'GET' ->
			SocketId = st_request:arg(Request, <<"id">>),
			if SocketId == undefined ->
				{error, 400, <<"A socket id must be supplied when connecting a jquery socket.">>};
			true ->
				case st_request:arg(Request, <<"transport">>) of
					<<"ws">> ->
						Connection = stutil:bstr_to_lower(st_request:get_header(Request, 'Connection', <<"undefined">>)),
						case Connection of
							<<"upgrade">> ->
								% Check the upgrade type
								Upgrade = stutil:bstr_to_lower(st_request:get_header(Request, 'Upgrade', <<"undefined">>)),
								if Upgrade == <<"websocket">> ->
									st_jqs:new_websocket(SocketId, Request, Options, CallBacks, AllowOrigin);
								true ->
									{error, 400, <<"Invalid upgrade request">>}
								end;
							_ ->
								{error, 400, <<"Invalid upgrade request for a web socket">>}
						end;

					<<"sse">> ->
						st_jqs:new_stream(sse, SocketId, Request, Options, CallBacks, AllowOrigin);

					<<"streamiframe">> ->
						st_jqs:new_stream(streamiframe, SocketId, Request, Options, CallBacks, AllowOrigin);

					<<"streamxdr">> ->
						st_jqs:new_stream(streamxdr, SocketId, Request, Options, CallBacks, AllowOrigin);

					<<"streamxhr">> ->
						st_jqs:new_stream(streamxhr, SocketId, Request, Options, CallBacks, AllowOrigin);

					Other ->
						io:format("Unknown transport type ~p~n", [Other]),
						{error, 400, <<"Unknown transport type.">>}
				end
			end;

		'POST' ->
			AllowSize = stutil:size_to_bytes(proplists:get_value(max_post_data, Options, {64, kb})),
			Timeout = proplists:get_value(timeout, Options, 60) * 1000,
			ContentLength = st_request:content_length(Request),
			ContentRead = st_request:content_read(Request),
			if AllowSize >= ContentLength, ContentRead == 0 ->
				NewRequest = st_request:process_post_data(Request, Timeout),
				st_jqs:stream_post(NewRequest);
			true ->
				{error, 400, <<"Post data too long.">>}
			end;

		Other ->
			io:format("Invalid jquery socket request method of ~p~n", [Other]),
			{error, 400, <<"Invalid request method.">>}
	end;

% Websocket upgrade request
rule(Rst, [{web_socket, AllowOrigin, Options, CallBacks} | Rules], Request) ->
	Connection = stutil:bstr_to_lower(st_request:get_header(Request, 'Connection', <<"undefined">>)),
	case Connection of
		<<"upgrade">> ->
			% Check the upgrade type
			Upgrade = stutil:bstr_to_lower(st_request:get_header(Request, 'Upgrade', <<"undefined">>)),
			if Upgrade == <<"websocket">> ->
				% Check the origin is authorised
				Authorised = st_websocket:authorise(st_request:get_header(Request, <<"origin">>, undefined), AllowOrigin),
				if Authorised ->
					st_websocket:connect(Request, Options, CallBacks);
				true ->
					io:format("Websocket invalid origin: ~p~n", [st_request:get_header(Request, <<"origin">>, undefined)]),
					{error, 403, <<"Invalid Origin">>}
				end;
			true ->
				rule(Rst, Rules, Request)
			end;
		_ ->
			rule(Rst, Rules, Request)
	end;

% Debug line
rule(Rst, [{debug, Call} | Rules], Request) ->
	case Call of
		_ when is_binary(Call) ->
			io:format("> Debug: ~s~n", [Call]);
		_ ->
			io:format("Unknown debug call: ~p~n", [Call])
	end,
	rule(Rst, Rules, Request);

% Out of rules...
rule(Rst, [], Request) ->
	case st_request:method(Request) of
		'ERROR' ->
			% Default error message
			StatusCode = stutil:to_integer(st_request:url(Request)),
			ErrorMsg = stutil:to_binary(st_request:arg(Request, <<"error">>, <<"Unknown Error">>)),
			ErrorDetail = stutil:to_binary(st_request:arg(Request, <<"detail">>, <<"Unknown error.">>)),
			{ok, Response} = st_response:new(
					Request, StatusCode, [],
					<<"<html><head><title>", (stutil:http_status_code(StatusCode))/binary, "</title></head>",
					"<body><h1>", (stutil:http_status_code(StatusCode))/binary, "</h1><h3>", ErrorMsg/binary,
					"</h3><div>", ErrorDetail/binary, "</div></body></html>">>
				),
			{send, finalise_response(Response, Rst, Request)};

		_ ->
			ErrDetail = <<"Could not find the page: ", (st_request:url(Request))/binary>>,
			{error, 404, ErrDetail}
	end;

% Other rule
rule(Rst, [Rule | _Rules], Request) ->
	io:format("Unknown rule ~p~n", [Rule]),
	{ok, Response} = st_response:new(Request, 500, [],
			stutil:to_binary(io_lib:format("<pre>Error unknown rule: ~p~n~n~p~n~n~p</pre>", [Rule, Rst, Request]))
		),
	{send, finalise_response(Response, Rst, Request)};

% Broken call
rule(_Rst, Rules, _Request) ->
	io:format("Bad call to rule() with rules: ~p~n", [Rules]),
	error.

%% ===================================================================
%% Support functions
%% ===================================================================


process_symfony_request(Rst, Options, Request, PostData) ->
	BaseDir = Rst#rst.path,
	DefaultFile = proplists:get_value(default_path, Options, <<"app.php">>),

	FileParts = case Rst#rst.url_parts of
		[] -> [DefaultFile];
		[<<>>] -> [DefaultFile];
		Parts -> Parts
	end,

	ExecExtension = proplists:get_value(exec_extension, Options, <<".php">>),

	% io:format("Path is ~p, path_contains returns ~p~n", [FileParts, path_contains(FileParts, ExecExtension, [])]),

	case path_contains(FileParts, ExecExtension, []) of
		{match, ScriptParts, _RestParts} ->
			% Matched a PHP script, so execute that passing in the remaining path information
			ScriptPath = combine_url_path(ScriptParts, BaseDir),
			% RestPath = combine_url_path(RestParts, <<>>),
			case filelib:is_regular(ScriptPath) of
				true ->
					FCGI = st_fcgi:new(proplists:get_value(connect, Options, [])),
					% FCGI_Params = fcgi_params(FCGI, Rst, ScriptPath, Request, RestPath, Options),
					FCGI_Params = fcgi_params(FCGI, ScriptPath, Rst, Request, [
							{<<"SCRIPT_NAME">>, combine_url_path(ScriptParts, <<>>)}
						]),
					FCGI_Post = if PostData == <<>> ->
						FCGI_Params;
					true ->
						st_fcgi:stdin(FCGI_Params, PostData)
					end,
					FCGI_End = st_fcgi:stdin_end(FCGI_Post),
					case st_fcgi:execute(Request, FCGI_End) of
						{ok, Response} ->
							{send, Response};
						{error, Reason} ->
							{error, 500, Reason}
					end;
				false ->
					{error, 404, not_found}
			end;
		false ->
			FileName = combine_url_path(FileParts, BaseDir),
			% io:format("FileName is ~p, serve_static_file is ~p~n", [FileName, serve_static_file(FileName, Options)]),
			case serve_static_file(FileName, Options) of
				true ->
					% Matched a static file
					MimeType = proplists:get_value(mime_type, Options, st_mime_type:get_type(filename:extension(FileName))),
					case file_modified(FileName, Request) of
						true ->
							{ok, Response} = st_response:new(Request, ok, [{<<"Content-Type">>, MimeType}], {file, FileName}),
							{send, finalise_response(Response, Rst, Request)};
						false ->
							{ok, Response} = st_response:new(Request, not_modified, [{<<"Content-Type">>, MimeType}], <<>>),
							{send, finalise_response(Response, Rst, Request)}
					end;
				false ->
					% io:format("passing to fcgi~n"),
					% Didn't match a static file, so send the request to Symfony
					FCGI = st_fcgi:new(proplists:get_value(connect, Options, [])),
					% FCGI_Params = fcgi_params(FCGI, Rst, combine_url_path([DefaultFile], BaseDir), Request, st_request:url(Request), Options),
					FCGI_Params = fcgi_params(FCGI, combine_url_path([DefaultFile], BaseDir), Rst, Request, [
						]),
					FCGI_Post = if PostData == <<>> ->
						FCGI_Params;
					true ->
						% io:format("Post Data is ~p~n", [PostData]),
						st_fcgi:stdin(FCGI_Params, PostData)
					end,
					FCGI_End = st_fcgi:stdin_end(FCGI_Post),
					case st_fcgi:execute(Request, FCGI_End) of
						{ok, Response} ->
							% io:format("FCGI gave a response~n"),
							{send, Response};
						{error, Reason} ->
							% io:format("FCGI had a error~n"),
							{error, 500, Reason}
					end
			end
	end.



fcgi_params(FCGI, Script, Rst, Request, ManualHeaders) ->
	BaseHeaders = [
		{<<"HTTP_", (binary:replace(stutil:bstr_to_upper(stutil:to_binary(Header)), <<$->>, <<$_>>, [global]))/binary>>, stutil:to_binary(Value)} 
			|| {Header, Value} <- st_request:get_headers(Request)
	],
	{ok, {{RawIp1, RawIp2, RawIp3, RawIp4}, Port}} = st_socket:peername(st_request:raw_socket(Request)),
	Settings = [
			{<<"QUERY_STRING">>, st_request:query_string(Request)},
			{<<"SCRIPT_FILENAME">>, Script},
			{<<"REQUEST_METHOD">>, stutil:to_binary(st_request:method(Request))},
			{<<"CONTENT_LENGTH">>, stutil:to_binary(st_request:content_length(Request))},
			{<<"CONTENT_TYPE">>, stutil:to_binary(st_request:get_header(Request, 'Content-Type', <<>>))},
			{<<"DOCUMENT_ROOT">>, stutil:to_binary(Rst#rst.path)},
			{<<"DOCUMENT_URI">>, stutil:to_binary(st_request:url(Request))},
			{<<"GATEWAY_INTERFACE">>, <<"CGI/1.1">>},
			{<<"HTTPS">>, case st_request:is_ssl(Request) of false -> <<"off">>; _ -> <<"on">> end},
			{<<"REQUEST_URI">>, st_request:request_uri(Request)},
			{<<"REDIRECT_STATUS">>, <<"200">>},
			{<<"REMOTE_ADDR">>, stutil:binary_join([stutil:to_binary(Ip) || Ip <- [RawIp1, RawIp2, RawIp3, RawIp4]], <<$.>>)},
			{<<"REMOTE_PORT">>, stutil:to_binary(Port)},
			{<<"SERVER_PROTOCOL">>, st_request:http_version_str(Request)},
			{<<"SERVER_SOFTWARE">>, <<"Stampede/1.0">>}
		] ++  BaseHeaders ++ ManualHeaders,
	st_fcgi:params_end(FCGI, Settings).


serve_static_file(FileName, Options) ->
	case filelib:is_regular(FileName) of
		true ->
			case lists:member(filename:extension(FileName), proplists:get_value(exclude_extensions, Options, [])) of
				true -> false;
				_ -> true
			end;
		false ->
			false
	end.

path_contains([Part | UrlParts], Extension, Acc) when byte_size(Part) < byte_size(Extension) ->
	path_contains(UrlParts, Extension, [Part | Acc]);
path_contains([Part | UrlParts], Extension, Acc) ->
	case binary:part(Part, byte_size(Part) - byte_size(Extension), byte_size(Extension)) of
		Extension -> {match, lists:reverse([Part | Acc]), UrlParts};
		_ -> path_contains(UrlParts, Extension, [Part | Acc])
	end;
path_contains([], _Extension, _Acc) ->
	false.


set_path(Rst, OrigPath) ->
	Path = case binary:last(OrigPath) of
		$/ -> binary:part(OrigPath, 0, byte_size(OrigPath) - 1);
		_ -> OrigPath
	end,
	Rst#rst{path = Path, base_path = Path}.

append_path(Rst, OrigPath) ->
	Path = case binary:last(OrigPath) of
		$/ -> binary:part(OrigPath, 0, byte_size(OrigPath) - 1);
		_ -> OrigPath
	end,
	Rst#rst{path = <<(Rst#rst.path)/binary, $/, Path/binary>>}.

split_url(<<$/, Url/binary>>) ->
	binary:split(Url, <<$/>>, [global]);
split_url(Url) ->
	binary:split(Url, <<$/>>, [global]).


url_match([Path | RestUrl], [Match | RestMatch], UrlArgs) ->
	case binary:first(Match) of
		$: when byte_size(Match) > 1 ->
			url_match(RestUrl, RestMatch, [{binary:part(Match, 1, byte_size(Match) - 1), Path} | UrlArgs]);
		$* ->
			url_match(RestUrl, RestMatch, UrlArgs);
		_ ->
			if Path == Match -> url_match(RestUrl, RestMatch, UrlArgs);
			true -> false end
	end;
url_match([], [], UrlArgs) ->
	{match, [], UrlArgs};
url_match(Url, [], UrlArgs) ->
	{match, Url, UrlArgs};
url_match([], _Match, _UrlArgs) ->
	false.


combine_url_path([Part | Rest], Acc) ->
	combine_url_path(Rest, <<Acc/binary, $/, Part/binary>>);
combine_url_path([], Acc) ->
	Acc.

file_modified(FileName, Request) ->
	case st_request:if_modified_since(Request) of
		undefined ->
			true;
		IfMod ->
			{ok, FileInfo} = file:read_file_info(FileName),
			% Nasty date hack to eliminate UTC conversion differences
			MTime = httpd_util:convert_request_date(httpd_util:rfc1123_date(FileInfo#file_info.mtime)),
			if MTime > IfMod -> true;
			true -> false end
	end.

finalise_response(Response, Rst, _Request) ->
	StatusCode = st_response:status_code(Response),
	if StatusCode == 200; StatusCode == ok ->
		case Rst#rst.browser_cache_for of
			undefined -> Response;
			_ ->
				{ok, NewResponse} = st_response:header(Response, <<"Expires">>, calc_cache_date(Rst#rst.browser_cache_for)),
				NewResponse
		end;
	true ->
		Response
	end.

calc_cache_date(Delta) ->
	CacheDate = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) + delta_to_seconds(Delta)),
	stutil:to_binary(httpd_util:rfc1123_date(CacheDate)).

delta_to_seconds({{Y, M, D}, {Hr, Min, Sec}}) ->
	Sec + (Min * 60) + (Hr * 3600) + (D * 86400) + (M * 2629800) + (Y * 31557600).

call_erlang(CallDetails, Rst, Request) ->
	case CallDetails of
		{call, ErlFun, Args} ->
			ErlFun(Request, Rst, Args)
	end.


gen_url([Part | Rest], Rst, Request, UrlAcc) ->
	case Part of
		{url_arg, Key} -> gen_url(Rest, Rst, Request, <<UrlAcc/binary, $/, (st_request:url_arg(Request, Key, <<$?>>))/binary>>);
		{get_arg, Key} -> gen_url(Rest, Rst, Request, <<UrlAcc/binary, $/, (st_request:arg(Request, Key, <<$?>>))/binary>>);
		{post_arg, Key} -> gen_url(Rest, Rst, Request, <<UrlAcc/binary, $/, (st_request:post_arg(Request, Key, <<$?>>))/binary>>);
		BinStr when is_binary(BinStr) -> gen_url(Rest, Rst, Request, <<UrlAcc/binary, "/", BinStr/binary>>)
	end;
gen_url([], _Rst, _Request, UrlAcc) ->
	UrlAcc.

