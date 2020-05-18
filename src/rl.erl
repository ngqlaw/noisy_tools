%%%-------------------------------------------------------------------
%%% @author ngq <ngq_scut@126.com>
%%% @copyright (C) 2016, ngq <ngq_scut@126.com>.
%%% @doc
%%% 热更新模块
%%%
%%% @end
%%% Created : 09. 七月 2016 09:55
%%%-------------------------------------------------------------------
-module(rl).
-author("ngq").

%% API
-export([
    load_from_source_bin/1,
    find_ebin_by_dir/1,
    get_beam_file/1,
    u/0, u/1
]).

%% 根据erl文件内容加载模块
load_from_source_bin(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    TokensList = split_tokens_by_dot(Tokens, [], []),
    AbsForms = [begin
        {ok, AbsForm} = erl_parse:parse_form(Ts),
        AbsForm
    end || Ts <- TokensList],
    {ok, DataModule, Binary} = compile:forms(AbsForms),
    code:load_binary(DataModule, cover_compiled, Binary).

split_tokens_by_dot([{dot, _} = Dot|T], Temp, Res) ->
    split_tokens_by_dot(T, [], [lists:reverse([Dot|Temp])|Res]);
split_tokens_by_dot([H|T], Temp, Res) ->
    split_tokens_by_dot(T, [H|Temp], Res);
split_tokens_by_dot([], _, Res) ->
    lists:reverse(Res).


%% 更新所有修改的beam文件
u() ->
    {ok, Path} = file:get_cwd(),
    CodePaths = find_ebin_by_dir(Path),
    All = get_beam_file(CodePaths, []),
    L = code:all_loaded(),
    do(All, L, []).

do([{Module, Path}|T], L, Res) ->
    case lists:keytake(Module, 1, L) of
        {value, _, NewL} ->   %% 已加载
            OldMD5 = Module:module_info(md5),
            {ok, {_, NewMD5}} = beam_lib:md5(Path),
            case OldMD5 == NewMD5 of
                true ->
                    do(T, NewL, Res);
                false ->
                    do(T, NewL, [Module|Res])
            end;
        false ->
            do(T, L, Res)
    end;
do([], _L, Res) ->
    u(Res).

u(Module) when is_atom(Module) ->
    u([Module]);
u(L) when is_list(L) ->
    purged_reload(L, self()),
    ok_reload(L, []).

purged_reload([Module|T], Parents) ->
    spawn_link(fun() ->
        Reply = do_purged_reload(Module, 10),
        Parents ! {Reply, Module}
               end),
    purged_reload(T, Parents);
purged_reload([], _Parents) ->
    ok.

do_purged_reload(Module, N) when N > 0 ->
    case code:soft_purge(Module) of
        true ->
            code:load_file(Module),
            true;
        false ->
            timer:sleep(1000),
            do_purged_reload(Module, N - 1)
    end;
do_purged_reload(Module, _N) ->
    case code:purge(Module) of
        true ->
            code:load_file(Module),
            true;
        false ->
            false
    end.

ok_reload([_|_] = L, Res) ->
    receive
        {Reply, Module} ->
            NewL = lists:delete(Module, L),
            ok_reload(NewL, [{Module, Reply}|Res])
    end;
ok_reload([], Res) ->
    Res.


%% 查询目录下所以ebin文件夹
find_ebin_by_dir(Dirs) ->
    find_ebin_by_dir(Dirs, []).

find_ebin_by_dir([Dir|T], Res) ->
    case file:list_dir(Dir) of
        {ok, L} ->
            NewRes = do_find_ebin_by_dir(L, Dir, Res),
            find_ebin_by_dir(T, NewRes);
        _E ->
            find_ebin_by_dir(T, Res)
    end;
find_ebin_by_dir([], Res) ->
    Res.

do_find_ebin_by_dir(["ebin"|T], Dir, Res) ->
    SubDir = filename:join([Dir, "ebin"]),
    case filelib:is_dir(SubDir) of
        true ->
            do_find_ebin_by_dir(T, Dir, [SubDir|Res]);
        false ->
            do_find_ebin_by_dir(T, Dir, Res)
    end;
do_find_ebin_by_dir([H|T], Dir, Res) ->
    SubDir = filename:join([Dir, H]),
    case filelib:is_dir(SubDir) of
        true ->
            NewRes = find_ebin_by_dir([SubDir], Res),
            do_find_ebin_by_dir(T, Dir, NewRes);
        false ->
            do_find_ebin_by_dir(T, Dir, Res)
    end;
do_find_ebin_by_dir([], _Dir, Res) ->
    Res.

%% 查询目录下所有beam文件
get_beam_file(Paths) ->
    get_beam_file(Paths, []).

get_beam_file([H|T], Res) ->
    case file:list_dir(H) of
        {ok, L} ->
            NewRes = do_get_beam_file(L, H, Res),
            get_beam_file(T, NewRes);
        _E ->
            get_beam_file(T, Res)
    end;
get_beam_file([], Res) ->
    lists:usort(Res).

do_get_beam_file([H|T], Dir, Res) ->
    case filename:extension(H) == ".beam" of
        true ->
            Module = list_to_atom(filename:basename(H, ".beam")),
            Path = filename:join([Dir, H]),
            do_get_beam_file(T, Dir, [{Module, Path}|Res]);
        false ->
            SubDir = filename:join([Dir, H]),
            case filelib:is_dir(SubDir) of
                true ->
                    NewRes = get_beam_file([SubDir], Res),
                    do_get_beam_file(T, Dir, NewRes);
                false ->
                    do_get_beam_file(T, Dir, Res)
            end
    end;
do_get_beam_file([], _Dir, Res) ->
    Res.
