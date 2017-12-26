%%%-----------------------------------
%%% @Module  : lib_math
%%% @author: ninggq <ngq_scut@126.com>
%%% @Description: math
%%%-----------------------------------
-module(lib_math).

-export([combine/2, combine_action/3]).

%% 排列组合
combine([_|_] = List, N) when is_integer(N) ->
  do_combine(List, N, []);
combine([], _) ->
  [].

do_combine([H|T], N, Res) when N > 1 ->
  SubRes = sub_combine(T, N - 1),
  NewRes = [[H|R] || R <- SubRes],
  do_combine(T, N, NewRes ++ Res);
do_combine(List, 1, Res) ->
  [[R] || R <- List] ++ Res;  
do_combine([], _N, Res) ->
  Res.

sub_combine(List, N) when N > 1 ->
  do_combine(List, N, []);
sub_combine(List, 1) ->
  [[R] || R <- List].

%% 排列组合结果进行操作
combine_action([_|_] = List, N, Fun) when is_integer(N) andalso is_function(Fun, 1) ->
  do_combine_action(List, N, Fun);
combine_action([], _N, _Fun) ->
  ok.  

do_combine_action([H|T], N, Fun) when N > 1 ->
  SubRes = sub_combine(T, N - 1),
  [Fun([H|R]) || R <- SubRes],
  do_combine_action(T, N, Fun);
do_combine_action(List, 1, Fun) ->
  [Fun([R]) || R <- List],
  ok; 
do_combine_action([], _N, _Fun) ->
  ok.
