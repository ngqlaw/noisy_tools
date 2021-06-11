%%%-------------------------------------------------------------------
%%% @author ngq <ngq_scut@126.com>
%%% @doc
%%% math
%%% @end
%%%-------------------------------------------------------------------
-module(lib_math).

-export([
    combine/2,
    combine_action/3,
    list_combine/1
]).

%% @doc 排列组合
-spec(combine(list(), integer()) -> [list()]).
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

%% @doc 排列组合结果进行操作
-spec(combine_action(list(), N::integer(), Fun) -> ok when
    Fun::fun((R::term()) -> term())).
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

%% @doc
%% multi list combine
%% for example:
%% list_combine([[1,2], [3,4]]) ->
%%   [[4,2],[4,1],[3,2],[3,1]].
%% @end
-spec(list_combine([list()]) -> [list()]).
list_combine(List) when is_list(List) ->
    lists:foldl(
        fun(L, Acc0) ->
            lists:foldl(
                fun(Add, Acc1) ->
                    list_combine(Acc0, Add) ++ Acc1
                end, [], L
            )
        end, [], List
    ).

list_combine([_|_] = List, Add) ->
    [[Add|H] || H <- List];
list_combine([], Add) ->
    [[Add]].
