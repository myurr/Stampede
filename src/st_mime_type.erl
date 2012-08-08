-module(st_mime_type).

-export([get_type/1]).

get_type(<<".html">>) ->
	<<"text/html">>;
get_type(<<".css">>) ->
	<<"text/css">>;
get_type(<<".txt">>) ->
	<<"text/plain">>;
get_type(<<".js">>) ->
	<<"application/x-javascript">>;
get_type(<<".jpg">>) ->
	<<"image/jpeg">>;
get_type(<<".jpeg">>) ->
	<<"image/jpeg">>;
get_type(<<".gif">>) ->
	<<"image/gif">>;
get_type(<<".png">>) ->
	<<"image/png">>;
get_type(<<".xml">>) ->
	<<"application/xml">>;
get_type(<<".mp3">>) ->
	<<"audio/mpeg3">>;
get_type(Type) ->
	io:format("Unknown MIME type: ~p~n", [Type]),
	<<"text/html">>.
