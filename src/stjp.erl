-module(stjp).

-export([get/2, get/3]).

get(Json, PathStr) ->
	get(Json, PathStr, undefined).

get(Json, PathStr, Default) ->
	Path = split_path(stutil:to_binary(PathStr), [], <<>>),
	js_get(Json, Path, Default).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Support Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_path(<<$., Str/binary>>, Path, Acc) ->
	split_path(Str, [{property, Acc} | Path], <<>>);
split_path(<<Chr, Str/binary>>, Path, Acc) ->
	split_path(Str, Path, <<Acc/binary, Chr>>);
split_path(<<>>, Path, <<>>) ->
	lists:reverse(Path);
split_path(<<>>, Path, Acc) ->
	lists:reverse([{property, Acc} | Path]).


js_get({struct, Vals}, [{property, Key} | Path], Default) ->
	Val = proplists:get_value(Key, Vals, Default),
	js_get(Val, Path, Default);
js_get(Json, [], _Default) ->
	Json;
js_get(_Json, _Path, Default) ->
	Default.
