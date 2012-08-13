-module(st_routing).

% Exported API
-export([route/2]).

%% ===================================================================
%% Definitions
%% ===================================================================

-include_lib("kernel/include/file.hrl").

-record(rst, {path = undefined, base_path = undefined, url_parts = [], browser_cache_for = undefined}).

%% ===================================================================
%% API functions
%% ===================================================================

%% ====================
%% Route a request
%% ====================

route(Request, Rules) ->
	Rst = #rst{url_parts = split_url(st_request:url(Request))},
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

% Match a URL
rule(Rst, [{url, MatchRule, SubRules} | Rules], Request) ->
	% io:format("Url match: ~p vs ~p = ~p~n", [Rst#rst.url_parts, split_url(MatchRule), url_match(Rst#rst.url_parts, split_url(MatchRule))]),
	case url_match(Rst#rst.url_parts, split_url(MatchRule)) of
		{match, Url} ->
			rule(Rst#rst{url_parts = Url}, SubRules, Request);
		false ->
			rule(Rst, Rules, Request)
	end;

% Map a URL to a fixed file
rule(Rst, [{map_file, MatchRule, ServeFileName, Options} | Rules], Request) ->
	case url_match(Rst#rst.url_parts, split_url(MatchRule)) of
		{match, []} ->
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

% Match a static directory of content
rule(Rst, [{static_dir, DefaultFile, Options} | Rules], Request) ->
	BaseDir = Rst#rst.path,
	FileName = case Rst#rst.url_parts of
		[] -> <<BaseDir/binary, $/, DefaultFile/binary>>;
		[<<>>] -> <<BaseDir/binary, $/, DefaultFile/binary>>;
		Parts -> combine_url_path(Parts, BaseDir)
	end,
	case filelib:is_regular(FileName) of
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
rule(Rst, [{browser_cache_for, undefined} | Rules], Request) ->
	rule(Rst#rst{browser_cache_for = undefined}, Rules, Request);
rule(Rst, [{browser_cache_for, Delta} | Rules], Request) ->
	rule(Rst#rst{browser_cache_for = Delta}, Rules, Request);

% Send some static content
rule(Rst, [{static, Content} | _Rules], Request) ->
	{ok, Response} = st_response:new(Request, ok, [], Content),
	{send, finalise_response(Response, Rst, Request)};

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
	io:format("Unknown rule~n"),
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

url_match([Path | RestUrl], [Match | RestMatch]) ->
	if Path == Match -> url_match(RestUrl, RestMatch);
	true -> false end;
url_match([], []) ->
	{match, []};
url_match(Url, []) ->
	{match, Url};
url_match([], _Match) ->
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
