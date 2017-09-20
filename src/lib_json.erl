%%%-----------------------------------
%%% @Module  : lib_json
%%% @author: ningguoqiang <ngq_scut@126.com>
%%% @Description: json
%%%-----------------------------------
-module(lib_json).

-export([encode/1, decode/1]).

%% @doc 生成Jason
encode(L) ->
	case encode_value(L) of
		square_bracket ->
			encode_square_bracket(L, []);
		curly_bracket ->
			encode_curly_bracket(L, [])
	end.

%% 花括号编码
encode_curly_bracket([{K, V}|T], Res) ->
	Key = encode_string(K),
	case encode_value(V) of
		square_bracket ->
			Value = [encode_square_bracket(V, [])];
		curly_bracket ->
			Value = [encode_curly_bracket(V, [])];
		Value ->
			ok
	end,
	encode_curly_bracket(T, [","] ++ Value ++ [":"] ++ Key ++ Res);
encode_curly_bracket([], []) ->
	"{}";
encode_curly_bracket([], [_|Res]) ->
	"{" ++ lists:concat(lists:reverse(Res)) ++ "}".

%% 方括号编码
encode_square_bracket([V|T], Res) ->
	case encode_value(V) of
		square_bracket ->
			Value = [encode_square_bracket(V, [])];
		curly_bracket ->
			Value = [encode_curly_bracket(V, [])];
		Value ->
			ok
	end,
	encode_square_bracket(T, Value ++ Res);
encode_square_bracket([], Res) ->
	"[" ++ lists:concat(lists:reverse(Res)) ++ "]".

%% 编码值
encode_value(S) when is_integer(S) orelse is_float(S) ->
	[util:to_list(S)];
encode_value(false) ->
	["false"];
encode_value(null) ->
	["null"];
encode_value(true) ->
	["true"];
encode_value(S) ->
	encode_string(S).

encode_string(S) when is_list(S) ->
	case lists:all(fun(Tuple) -> is_tuple(Tuple) end, S) of
		true ->
			curly_bracket;
		false ->
			case lists:all(fun(I) -> is_integer(I) end, S) of
				true ->
					["\"", S, "\""];
				false ->
					square_bracket
			end
	end;
encode_string(S) ->
	["\"", util:to_list(S), "\""].

%% @doc 解析Jason
decode(<<>>) ->
	[];
decode(Bin) when is_binary(Bin) ->
	B = begin_decode(Bin),
	case decode_value(B) of
		{curly_bracket, B1} ->
			{Res, _} = decode_curly_bracket(B1);
		{square_bracket, B1} ->
			{Res, _} = decode_square_bracket(B1)
	end,
	Res;
decode(L) when is_list(L) ->
	decode(unicode:characters_to_binary(L)).

%% 移除头部无效数据
begin_decode(<<"{", _/binary>> = B) ->
	B;
begin_decode(<<"[", _/binary>> = B) ->
	B;
begin_decode(<<_H:8, B/binary>>) ->
	begin_decode(B);
begin_decode(<<>>) ->
	<<>>.

%% 解析花括号内容
decode_curly_bracket(<<"{}", B/binary>>) ->
	{[], B};
decode_curly_bracket(<<"{", B/binary>>) ->
	do_decode_curly_bracket(B, []).

do_decode_curly_bracket(B, Res) ->
	{Key, <<":", B1/binary>>} = decode_string(B),
	case decode_value(B1) of
		{curly_bracket, B2} ->
			{Value, B3} = decode_curly_bracket(B2);
		{square_bracket, B2} ->
			{Value, B3} = decode_square_bracket(B2);
		{Value, B3} ->
			ok
	end,
	case check_curly_bracket_next(B3) of
		{'NEXT', B4} ->
			do_decode_curly_bracket(B4, [{util:to_list(Key), util:to_list(Value)}|Res]);
		{'END', B4} ->
			{lists:reverse([{util:to_list(Key), util:to_list(Value)}|Res]), B4}
	end.

check_curly_bracket_next(<<32:8, B/binary>>) ->
	check_curly_bracket_next(B);
check_curly_bracket_next(<<9:8, B/binary>>) ->
	check_curly_bracket_next(B);
check_curly_bracket_next(<<10:8, B/binary>>) ->
	check_curly_bracket_next(B);
check_curly_bracket_next(<<13:8, B/binary>>) ->
	check_curly_bracket_next(B);
check_curly_bracket_next(<<",", B/binary>>) ->
	{'NEXT', B};
check_curly_bracket_next(<<"}", B/binary>>) ->
	{'END', B}.

%% 解析方括号内容
decode_square_bracket(<<"[]", B/binary>>) ->
	{[], B};
decode_square_bracket(<<"[", B/binary>>) ->
	do_decode_square_bracket(B, []).

do_decode_square_bracket(B, Res) ->
	case decode_value(B) of
		{curly_bracket, B1} ->
			{Value, B2} = decode_curly_bracket(B1);
		{square_bracket, B1} ->
			{Value, B2} = decode_square_bracket(B1);
		{Value, B2} ->
			ok
	end,
	case check_square_bracket_next(B2) of
		{'NEXT', B3} ->
			do_decode_square_bracket(B3, [util:to_list(Value)|Res]);
		{'END', B3} ->
			{lists:reverse([util:to_list(Value)|Res]), B3}
	end.

check_square_bracket_next(<<32:8, B/binary>>) ->
	check_square_bracket_next(B);
check_square_bracket_next(<<9:8, B/binary>>) ->
	check_square_bracket_next(B);
check_square_bracket_next(<<10:8, B/binary>>) ->
	check_square_bracket_next(B);
check_square_bracket_next(<<13:8, B/binary>>) ->
	check_square_bracket_next(B);
check_square_bracket_next(<<",", B/binary>>) ->
	{'NEXT', B};
check_square_bracket_next(<<"]", B/binary>>) ->
	{'END', B}.

%% 取非Object和Array值
decode_value(<<"false", B/binary>>) ->
	{<<"false">>, B};
decode_value(<<"null", B/binary>>) ->
	{<<"null">>, B};
decode_value(<<"true", B/binary>>) ->
	{<<"true">>, B};
decode_value(<<"\"", B/binary>>) ->
	decode_string(<<"\"", B/binary>>);
decode_value(<<"-", H:8, B/binary>>) when H > 47 andalso H < 58 ->
	decode_number(<<"-", H:8, B/binary>>);
decode_value(<<H:8, B/binary>>) when H > 47 andalso H < 58 ->
	decode_number(<<H:8, B/binary>>);
decode_value(<<"{", _/binary>> = B) ->
	{curly_bracket, B};
decode_value(<<"[", _/binary>> = B) ->
	{square_bracket, B};
decode_value(<<32:8, B/binary>>) ->
	decode_value(B);
decode_value(<<9:8, B/binary>>) ->
	decode_value(B);
decode_value(<<10:8, B/binary>>) ->
	decode_value(B);
decode_value(<<13:8, B/binary>>) ->
	decode_value(B).

%% 字符串
decode_string(<<32:8, B/binary>>) ->
	decode_string(B);
decode_string(<<9:8, B/binary>>) ->
	decode_string(B);
decode_string(<<10:8, B/binary>>) ->
	decode_string(B);
decode_string(<<13:8, B/binary>>) ->
	decode_string(B);
decode_string(<<"\"", B/binary>>) ->
	do_decode_string(B, []).
do_decode_string(<<"\\\"", B/binary>>, Res) ->
	do_decode_string(B, ["\\\""|Res]);
do_decode_string(<<"\"", B/binary>>, Res) ->
	{util:to_binary(lists:reverse(Res)), B};
do_decode_string(<<H:8, B/binary>>, Res) ->
	do_decode_string(B, [H|Res]).

%% 数值
decode_number(<<"-", B/binary>>) ->
	do_decode_number(B, [<<"-">>]);
decode_number(B) ->
	do_decode_number(B, []).
do_decode_number(<<H:8, B/binary>>, Res) when H > 47 andalso H < 58 ->
	do_decode_number(B, [H|Res]);
do_decode_number(<<".", B/binary>>, [H|_] = Res) when H > 47 andalso H < 58 ->
	do_decode_number(B, [<<".">>|Res]);
do_decode_number(<<"e-", H1:8, B/binary>>, [<<".">>, H2|_] = Res)
	when H1 > 47 andalso H1 < 58 andalso H2 > 47 andalso H2 < 58 ->
	do_decode_number(B, [H1, <<"e-">>|Res]);
do_decode_number(<<"e-", H1:8, B/binary>>, [H2|_] = Res)
	when H1 > 47 andalso H1 < 58 andalso H2 > 47 andalso H2 < 58 ->
	do_decode_number(B, [H1, <<"e-">>|Res]);
do_decode_number(<<"e+", H1:8, B/binary>>, [<<".">>, H2|_] = Res)
	when H1 > 47 andalso H1 < 58 andalso H2 > 47 andalso H2 < 58 ->
	do_decode_number(B, [H1, <<"e+">>|Res]);
do_decode_number(<<"e+", H1:8, B/binary>>, [H2|_] = Res)
	when H1 > 47 andalso H1 < 58 andalso H2 > 47 andalso H2 < 58 ->
	do_decode_number(B, [H1, <<"e+">>|Res]);
do_decode_number(<<"E-", H1:8, B/binary>>, [<<".">>, H2|_] = Res)
	when H1 > 47 andalso H1 < 58 andalso H2 > 47 andalso H2 < 58 ->
	do_decode_number(B, [H1, <<"E-">>|Res]);
do_decode_number(<<"E-", H1:8, B/binary>>, [H2|_] = Res)
	when H1 > 47 andalso H1 < 58 andalso H2 > 47 andalso H2 < 58 ->
	do_decode_number(B, [H1, <<"E-">>|Res]);
do_decode_number(<<"E+", H1:8, B/binary>>, [<<".">>, H2|_] = Res)
	when H1 > 47 andalso H1 < 58 andalso H2 > 47 andalso H2 < 58 ->
	do_decode_number(B, [H1, <<"E+">>|Res]);
do_decode_number(<<"E+", H1:8, B/binary>>, [H2|_] = Res)
	when H1 > 47 andalso H1 < 58 andalso H2 > 47 andalso H2 < 58 ->
	do_decode_number(B, [H1, <<"E+">>|Res]);
do_decode_number(<<32:8, B/binary>>, Res) ->
	do_decode_number(B, Res);
do_decode_number(<<9:8, B/binary>>, Res) ->
	do_decode_number(B, Res);
do_decode_number(<<10:8, B/binary>>, Res) ->
	do_decode_number(B, Res);
do_decode_number(<<13:8, B/binary>>, Res) ->
	do_decode_number(B, Res);
do_decode_number(B, Res) ->
	{util:to_binary(lists:reverse(Res)), B}.
