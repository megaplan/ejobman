-module(ejobman_worker_spawn).
-export([spawn_one_worker/1]).
-include("ejobman.hrl").
%%-----------------------------------------------------------------------------
-spec spawn_one_worker(#ejm{}) -> {reference() | error, #ejm{}}.
%%
%% @doc checks for max number of workers. Spawns a new worker if possible.
%%
spawn_one_worker(#ejm{max_workers = Max, workers = Workers} = C) ->
    Len = length(Workers),
    if  Len < Max ->
            real_spawn_one_worker(C);
        true ->
            {error, C}
    end.
%%-----------------------------------------------------------------------------
-spec real_spawn_one_worker(#ejm{}) -> {reference() | error, #ejm{}}.
%%
%% @doc Spawns a new worker, stores its pid (and a ref) in a list,
%% returns the modified state.
%% @since 2011-07-21 18:00
%%
real_spawn_one_worker(C) ->
    Id = make_ref(),
    Child_config = make_child_config(C),
    StartFunc = {ejobman_long_worker, start_link, [Child_config]},
    Child = {Id, StartFunc, permanent, 1000, worker, [ejobman_long_worker]},
    Workers = C#ejm.workers,
    Res = supervisor:start_child(ejobman_long_supervisor, Child),
    mpln_p_debug:pr({?MODULE, 'real_spawn_one_worker res', ?LINE, Res},
        C#ejm.debug, run, 3),
    case Res of
        {ok, Pid} ->
            Ch = #chi{pid=Pid, id=Id, start=now()},
            {Id, C#ejm{workers = [Ch | Workers]}};
        {ok, Pid, _Info} ->
            Ch = #chi{pid=Pid, id=Id, start=now()},
            {Id, C#ejm{workers = [Ch | Workers]}};
        {error, _Reason} ->
            {error, C}
    end.
%%-----------------------------------------------------------------------------
%%
%% @doc creates config (proplist actually) for a child
%% @since 2011-07-21 18:00
%%
-spec make_child_config(#ejm{}) -> list().

make_child_config(C) ->
    [{debug, C#ejm.debug}]
.
%%-----------------------------------------------------------------------------
