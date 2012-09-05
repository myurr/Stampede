-module(stutil).

-export([init/0, to_binary/1, to_integer/1, timestamp/0, bstr_to_lower/1, bstr_to_upper/1, char_to_lower/1, char_to_upper/1,
		urldecode/1, http_status_code/1, make_list/1, random_string/1, trim_str/1, trim_front/1, trim_rear/1, size_to_bytes/1,
		binary_join/2]).

init() ->
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed(A,B,C),
	ok.


%%% Generate Random Strings

random_string(Length) ->
	random_string(Length, <<>>).
	
random_string(Length, Str) when byte_size(Str) >= Length ->
	Str;
random_string(Length, Str) ->
	Rnd = crypto:rand_uniform(1, 62),
	Char = map_char(Rnd),
	random_string(Length, <<Str/bytes, Char>>).

map_char(N) when N > 36 ->
	$A + N - 37;
map_char(N) when N > 10 ->
	$a + N - 11;
map_char(N) ->
	$0 + N - 1.



timestamp() ->
	{MegaSeconds, Seconds, _MS} = now(),
	(MegaSeconds * 1000000) + Seconds.


to_binary(Input) when is_binary(Input) ->
	Input;
to_binary(Input) when is_list(Input) ->
	iolist_to_binary(Input);
to_binary(Input) when is_integer(Input) ->
	iolist_to_binary(integer_to_list(Input));
to_binary(Input) when is_atom(Input) ->
	atom_to_binary(Input, utf8);
to_binary(Other) ->
	iolist_to_binary(io_lib:format("~p", [Other])).

to_integer(Input) when is_integer(Input) ->
	Input;
to_integer(Input) when is_binary(Input) ->
	list_to_integer(binary_to_list(Input));
to_integer(Input) when is_list(Input) ->
	list_to_integer(Input);
to_integer(Input) when is_atom(Input) ->
	list_to_integer(atom_to_list(Input)).


bstr_to_upper(Str) ->
	<< <<(char_to_upper(Char))>> || <<Char>> <= Str >>.

bstr_to_lower(Str) ->
	<< <<(char_to_lower(Char))>> || <<Char>> <= Str >>.


char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Char) -> Char.

char_to_upper($a) -> $A;
char_to_upper($b) -> $B;
char_to_upper($c) -> $C;
char_to_upper($d) -> $D;
char_to_upper($e) -> $E;
char_to_upper($f) -> $F;
char_to_upper($g) -> $G;
char_to_upper($h) -> $H;
char_to_upper($i) -> $I;
char_to_upper($j) -> $J;
char_to_upper($k) -> $K;
char_to_upper($l) -> $L;
char_to_upper($m) -> $M;
char_to_upper($n) -> $N;
char_to_upper($o) -> $O;
char_to_upper($p) -> $P;
char_to_upper($q) -> $Q;
char_to_upper($r) -> $R;
char_to_upper($s) -> $S;
char_to_upper($t) -> $T;
char_to_upper($u) -> $U;
char_to_upper($v) -> $V;
char_to_upper($w) -> $W;
char_to_upper($x) -> $X;
char_to_upper($y) -> $Y;
char_to_upper($z) -> $Z;
char_to_upper(Char) -> Char.

urldecode(BinStr) ->
	urldecode(BinStr, <<>>).

urldecode(<<$%, UC, LC, Rest/binary>>, Out) ->
	U = urlunhex(UC),
	L = urlunhex(LC),
	if U == error; L == error ->
		urldecode(<<UC, LC, Rest>>, <<Out/binary, $%>>);
	true ->
		urldecode(Rest, <<Out/binary, (U bsl 4 bor L)>>)
	end;
urldecode(<<$+, Rest/binary>>, Out) ->
	urldecode(Rest, <<Out/binary, $ >>);
urldecode(<<Chr, Rest/binary>>, Out) ->
	urldecode(Rest, <<Out/binary, Chr>>);
urldecode(<<>>, Out) ->
	Out.

urlunhex(Chr) when Chr >= $A, Chr =< $F -> Chr - $A + 10;
urlunhex(Chr) when Chr >= $a, Chr =< $f -> Chr - $a + 10;
urlunhex(Chr) when Chr >= $0, Chr =< $9 -> Chr - $0;
urlunhex(_) -> error.


make_list(Item) when is_list(Item) ->
	Item;
make_list(Item) ->
	[Item].

trim_str(Str) ->
	trim_rear(trim_front(Str)).

trim_front(<<$ , Str/binary>>) ->
	trim_front(Str);
trim_front(<<10, Str/binary>>) ->
	trim_front(Str);
trim_front(<<13, Str/binary>>) ->
	trim_front(Str);
trim_front(<<9, Str/binary>>) ->
	trim_front(Str);
trim_front(Str) ->
	Str.

trim_rear(Str) ->
	case binary:last(Str) of
		$  -> trim_rear(binary:part(Str, 0, byte_size(Str) - 1));
		10 -> trim_rear(binary:part(Str, 0, byte_size(Str) - 1));
		13 -> trim_rear(binary:part(Str, 0, byte_size(Str) - 1));
		9 -> trim_rear(binary:part(Str, 0, byte_size(Str) - 1));
		_ -> Str
	end.

size_to_bytes({Size, b}) ->
	Size;
size_to_bytes({Size, kb}) ->
	Size;
size_to_bytes({Size, mb}) ->
	Size * 1024 * 1024;
size_to_bytes({Size, gb}) ->
	Size * 1024 * 1024 * 1024.



%% ====================
%% HTTP Status codes
%% ====================

http_status_code(ok)			-> <<"200 OK">>;
http_status_code(found)			-> <<"302 Found">>;
http_status_code(not_modified)	-> <<"304 Not Modified">>;
http_status_code(bad_request)	-> <<"400 Bad Request">>;
http_status_code(unauthorized)	-> <<"401 Unauthorized">>;
http_status_code(forbidden)		-> <<"403 Forbidden">>;
http_status_code(not_found)		-> <<"404 Not Found">>;
http_status_code(error)			-> <<"500 Internal Server Error">>;

http_status_code(100)	-> <<"100 Continue">>;
http_status_code(101)	-> <<"101 Switching Protocols">>;
http_status_code(102)	-> <<"102 Processing">>;
http_status_code(200)	-> <<"200 OK">>;
http_status_code(201)	-> <<"201 Created">>;
http_status_code(202)	-> <<"202 Accepted">>;
http_status_code(203)	-> <<"203 Non-Authoritative Information">>;
http_status_code(204)	-> <<"204 No Content">>;
http_status_code(205)	-> <<"205 Reset Content">>;
http_status_code(206)	-> <<"206 Partial Content">>;
http_status_code(207)	-> <<"207 Multi-Status">>;
http_status_code(208)	-> <<"208 Already Reported">>;
http_status_code(226)	-> <<"226 IM Used">>;
http_status_code(300)	-> <<"300 Multiple Choices">>;
http_status_code(301)	-> <<"301 Moved Permanently">>;
http_status_code(302)	-> <<"302 Found">>;
http_status_code(303)	-> <<"303 See Other">>;
http_status_code(304)	-> <<"304 Not Modified">>;
http_status_code(305)	-> <<"305 Use Proxy">>;
http_status_code(306)	-> <<"306 Switch Proxy">>;
http_status_code(307)	-> <<"307 Temporary Redirect">>;
http_status_code(308)	-> <<"308 Permanent Redirect">>;
http_status_code(400)	-> <<"400 Bad Request">>;
http_status_code(401)	-> <<"401 Unauthorized">>;
http_status_code(402)	-> <<"402 Payment Required">>;
http_status_code(403)	-> <<"403 Forbidden">>;
http_status_code(404)	-> <<"404 Not Found">>;
http_status_code(405)	-> <<"405 Method Not Allowed">>;
http_status_code(406)	-> <<"Not Acceptable">>;
http_status_code(407)	-> <<"407 Proxy Authentication Required">>;
http_status_code(408)	-> <<"408 Request Timeout">>;
http_status_code(409)	-> <<"409 Conflict">>;
http_status_code(410)	-> <<"410 Gone">>;
http_status_code(411)	-> <<"411 Length Required">>;
http_status_code(412)	-> <<"412 Precondition Failed">>;
http_status_code(413)	-> <<"413 Request Entity Too Large">>;
http_status_code(414)	-> <<"414 Request-URI Too Long">>;
http_status_code(415)	-> <<"415 Unsupported Media Type">>;
http_status_code(416)	-> <<"416 Request Range Not Satisfiable">>;
http_status_code(417)	-> <<"417 Expectation Failed">>;
http_status_code(420)	-> <<"420 Enhance Your Calm">>;
http_status_code(422)	-> <<"422 Unprocessable Entity">>;
http_status_code(423)	-> <<"423 Locked">>;
http_status_code(424)	-> <<"424 Failed Dependency">>;
http_status_code(425)	-> <<"425 Unordered Collection">>;
http_status_code(426)	-> <<"426 Upgrade Required">>;
http_status_code(428)	-> <<"428 Precondition Required">>;
http_status_code(429)	-> <<"429 Too Many Requests">>;
http_status_code(431)	-> <<"431 Request Header Fields Too Large">>;
http_status_code(500)	-> <<"500 Internal Server Error">>;
http_status_code(501)	-> <<"501 Not Implemented">>;
http_status_code(502)	-> <<"502 Bad Gateway">>;
http_status_code(503)	-> <<"503 Service Unavailable">>;
http_status_code(504)	-> <<"504 Gateway Timeout">>;
http_status_code(505)	-> <<"505 HTTP Version Not Supported">>;
http_status_code(506)	-> <<"506 Variant Also Negotiates">>;
http_status_code(507)	-> <<"507 Insufficient Storage">>;
http_status_code(508)	-> <<"508 Loop Detected">>;
http_status_code(509)	-> <<"509 Bandwidth Limit Exceeded">>;
http_status_code(510)	-> <<"510 Not Extended">>;
http_status_code(511)	-> <<"511 Network Authentication Required">>;
http_status_code(666)	-> <<"666 World Ended">>;
http_status_code(Err)	-> <<(stutil:to_binary(Err))/binary, " Unknown">>.


binary_join(Array, JoinStr) ->
	binary_join(Array, JoinStr, <<>>).

binary_join([Item | Rest], JoinStr, <<>>) ->
	binary_join(Rest, JoinStr, Item);
binary_join([Item | Rest], JoinStr, Acc) ->
	binary_join(Rest, JoinStr, <<Acc/binary, JoinStr/binary, Item/binary>>);
binary_join([], _JoinStr, Acc) ->
	Acc.
