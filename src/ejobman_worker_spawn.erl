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
    Child_config = make_child_config(C, Id),
    StartFunc = {ejobman_long_worker, start_link, [Child_config]},
    % for 'permanent' restart policy either worker or handler must contact
    % one another so handler keeps actual list of children. Or use gproc...
    % In the case of 'temporary' the handler does all the housekeeping
    Child = {Id, StartFunc, temporary, 1000, worker, [ejobman_long_worker]},
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
-spec make_child_config(#ejm{}, reference()) -> list().

make_child_config(C, Ref) ->
    [{id, Ref} | C#ejm.worker_config]
.
%%-----------------------------------------------------------------------------
