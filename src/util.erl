%%%-------------------------------------------------------------------
%%% @author ngq <ngq_scut@126.com>
%%% @copyright (C) 2016, ngq <ngq_scut@126.com>.
%%% @doc
%%%
%%% @end
%%% Created : 25. 六月 2016 14:17
%%%-------------------------------------------------------------------
-module(util).
-author("ngq").

%% API
%% 时间函数
-export([
  unixtime/0
  ,unixtime/1
  ,longunixtime/0
  ,zero_unixtime/0
  ,week_zero_unixtime/0
  ,month_zero_unixtime/0
  ,to_datetime/1
  ,is_same_date/2
  ,is_same_date/3
  ,is_same_week/2
  ,is_same_month/2
  ,get_next_month/1
]).
%% 值格式函数
-export([
  to_integer/1
  ,to_float/1
  ,to_num/1
  ,to_atom/1
  ,to_list/1
  ,to_term/1
  ,to_binary/1
  ,is_num/1
  ,is_letter/1
]).
-export([
  rand/1
  ,rand/2
  ,md5/1
]).

%% @doc 时间戳(秒)
unixtime() ->
  erlang:system_time(second).

unixtime(DateTime) ->
  [DT] = calendar:local_time_to_universal_time_dst(DateTime),
  calendar:datetime_to_gregorian_seconds(DT) - 62167219200.

%% @doc 时间戳(毫秒)
longunixtime() ->
  erlang:system_time(millisecond).

%% @doc 当天零点时间戳
zero_unixtime() ->
  {D, _} = calendar:local_time(),
  unixtime({D, {0, 0, 0}}).

%% @doc 周一零点时间戳
week_zero_unixtime() ->
  {D, _} = calendar:local_time(),
  N = calendar:day_of_the_week(D),
  T = unixtime({D, {0, 0, 0}}),
  T - (N - 1) * 86400.

%% @doc 1号零点时间戳
month_zero_unixtime() ->
  {{Y, M, _}, _} = calendar:local_time(),
  unixtime({{Y, M, 1}, {0, 0, 0}}).

%% @doc 将当前时间戳转成datetime格式
to_datetime(Seconds) when is_integer(Seconds) ->
  DT = calendar:gregorian_seconds_to_datetime(Seconds + 62167219200),
  calendar:universal_time_to_local_time(DT);
to_datetime(_) ->
  to_datetime(0).

%% @doc 判断两个时间戳是否同一天(0点)
is_same_date(T1, T2) ->
  {Date, _} = to_datetime(T1),
  case to_datetime(T2) of
    {Date, _} -> true;
    _ -> false
  end.

%% @doc 是否同一天(可设置时间点判断)
is_same_date(T1, T2, {0, 0, 0}) ->
  is_same_date(T1, T2);
is_same_date(Timestamp1, Timestamp2, Time) ->
  {D1, _T1} = util:to_datetime(Timestamp1),
  Zero1 = util:unixtime({D1, {0, 0, 0}}),
  {D2, _T2} = util:to_datetime(Timestamp2),
  Zero2 = util:unixtime({D2, {0, 0, 0}}),
  Base1 = util:unixtime({D1, Time}),
  Base2 = util:unixtime({D2, Time}),
  case Zero1 == Zero2 of
    true ->
      (Timestamp1 >= Base1 andalso Timestamp2 >= Base1)
        orelse (Timestamp1 =< Base1 andalso Timestamp2 =< Base1);
    false ->
      Timestamp1 > Base1 andalso Timestamp2 < Base2
  end.

%% @doc 判断两个时间戳是否同一个星期(0点)
is_same_week(T1, T2) ->
  {D1, _} = to_datetime(T1),
  N1 = calendar:day_of_the_week(D1),
  Time1 = unixtime({D1, {0, 0, 0}}),
  Z1 = Time1 - (N1 - 1) * 86400,
  {D2, _} = to_datetime(T2),
  N2 = calendar:day_of_the_week(D2),
  Time2 = unixtime({D2, {0, 0, 0}}),
  Z2 = Time2 - (N2 - 1) * 86400,
  Z1 == Z2.

%% @doc 判断两个时间戳是否同一个月(0点)
is_same_month(T1, T2) ->
  {{Y, M, _}, _} = to_datetime(T1),
  case to_datetime(T2) of
    {{Y, M, _}, _} -> true;
    _ -> false
  end.

%% @doc 获取下个月
get_next_month(12) -> 1;
get_next_month(M) when M > 0 andalso M < 12 -> M + 1.

%% @doc 转整数
to_integer(Integer) when is_integer(Integer) -> Integer;
to_integer(Float) when is_float(Float) -> erlang:trunc(Float);
to_integer(Any) -> to_integer(to_num(Any)).

%% @doc 转浮点数
to_float(Integer) when is_integer(Integer) -> Integer * 1.0;
to_float(Float) when is_float(Float) -> Float;
to_float(Any) -> to_float(to_num(Any)).

%% @doc 转数字
to_num(Integer) when is_integer(Integer) -> Integer;
to_num(Float) when is_float(Float) -> Float;
to_num(List) when is_list(List) ->
  case catch list_to_integer(List) of
    Integer when is_integer(Integer) -> Integer;
    _ ->
      case catch list_to_float(List) of
        Float when is_float(Float) -> Float;
        _ -> 0
      end
  end;
to_num(Binary) when is_binary(Binary) ->
  case catch binary_to_integer(Binary) of
    Integer when is_integer(Integer) -> Integer;
    _ ->
      case catch binary_to_float(Binary) of
        Float when is_float(Float) -> Float;
        _ -> 0
      end
  end;
to_num(_) ->
    0.

%% @doc 转成Erlang原子
to_atom(Atom) when is_atom(Atom) -> Atom;
to_atom(List) when is_list(List) -> list_to_atom(List);
to_atom(Binary) when is_binary(Binary) -> to_atom(to_term(Binary));
to_atom(Any) -> binary_to_atom(list_to_binary(util:to_list(Any)), utf8).

%% @doc 转列表格式
to_list(List) when is_list(List) -> List;
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_list(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
% to_list(Float) when is_float(Float) ->
%     float_to_list(Float, [{decimals, 2}]);
to_list(Any) -> lists:flatten(io_lib:format("~p", [Any])).

%% @doc 转成Erlang支持的数据格式(没有二进制格式),优先级从高到低 integer -> float -> atom -> tuple -> list
to_term(String) when is_list(String) ->
  {ok, Tokens, _} = erl_scan:string(String),
  NewTokens = case lists:reverse(Tokens) of
    [{dot, 1}|_] -> Tokens;
    Reverse -> lists:reverse([{dot, 1}|Reverse])
  end,
  case erl_parse:parse_term(NewTokens) of
    {ok, Value} -> Value;
    _ -> String
  end;
to_term(Binary) when is_binary(Binary) -> to_term(bitstring_to_list(Binary));
to_term(Integer) when is_integer(Integer) -> Integer;
to_term(Float) when is_float(Float) -> Float;
to_term(Atom) when is_atom(Atom) -> Atom;
to_term(Tuple) when is_tuple(Tuple) -> Tuple.

%% @doc 转成二进制
to_binary(B) when is_binary(B) -> B;
to_binary(Term) ->
  L = to_list(Term),
  case catch list_to_binary(L) of
    B when is_binary(B) ->
      B;
    _ ->
      [S] = io_lib:format("~p", [Term]),
      list_to_binary(S)
  end.

%% @doc 数字字符
is_num(C) when C > 47 andalso C < 58 -> true;
is_num(_) -> false.

%% @doc 通用字符
is_letter(C) when C > 96 andalso C < 123 -> true;
is_letter(C) when C > 64 andalso C < 91 -> true;
is_letter(_) -> false.

%% @doc 随机取列表中一个元素或1到N之间的整数
rand([_|_] = L) ->
  N = rand(1, length(L)),
  lists:nth(N, L);
rand(N) when is_integer(N) ->
  rand(1, N).

%% @doc 两个值(整数或浮点数)之间的随机数
rand(N1, N2) when (is_integer(N1) orelse is_float(N1)) andalso (is_integer(N2) orelse is_float(N2)) ->
    do_rand(N1, N2).
do_rand(N, N) ->
    N;
do_rand(Min, Max) when Min < Max ->
  case get('proc_rand_seed') of
    undefined ->
      Ran = rand:seed(exs1024, {erlang:phash2([node()]),
        erlang:monotonic_time(),
        erlang:unique_integer()}),
      put('proc_rand_seed', Ran);
    _ ->
      skip
  end,
  R = rand:uniform(),
  N = Max - Min,
  case is_integer(N) of
    true -> Min + erlang:round(R * N);
    false -> Min + R * N
  end;
do_rand(Max, Min) ->
  do_rand(Min, Max).

-spec md5(Data :: term()) -> Result::string().
md5(Term) ->
  L = do_md5(erlang:md5(to_binary(Term)), []),
  lists:flatten(L).
do_md5(<<A:4, Rest/bitstring>>, R) ->
  do_md5(Rest, [io_lib:format("~.16B", [A])|R]);
do_md5(<<>>, R) ->
  lists:reverse(R).
