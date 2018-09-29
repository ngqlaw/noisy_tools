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
-export([u/0, u/1]).

u() ->
  L = code:all_loaded(),
  CodePaths = lists:delete(".", code:get_path()),
  All = get_all_beam_file(CodePaths, []),
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

%% 查询目录下所有beam文件
get_all_beam_file([H|T], Res) ->
  case file:list_dir(H) of
    {ok, L} ->
      NewRes = get_beam_file(L, H, Res),
      get_all_beam_file(T, NewRes);
    _E ->
      get_all_beam_file(T, Res)
  end;
get_all_beam_file([], Res) ->
  lists:usort(Res).

get_beam_file([H|T], Dir, Res) ->
  case filename:extension(H) == ".beam" of
    true ->
      Module = list_to_atom(filename:basename(H, ".beam")),
      Path = filename:join([Dir, H]),
      get_beam_file(T, Dir, [{Module, Path}|Res]);
    false ->
      SubDir = filename:join([Dir, H]),
      case filelib:is_dir(SubDir) of
        true ->
          NewRes = get_all_beam_file([SubDir], Res),
          get_beam_file(T, Dir, NewRes);
        false ->
          get_beam_file(T, Dir, Res)
      end
  end;
get_beam_file([], _Dir, Res) ->
  Res.
