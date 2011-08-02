-module(ejobman_worker_spawn).
-export([spawn_one_worker/2]).
-include("ejobman.hrl").
%%-----------------------------------------------------------------------------
%%
%% @doc checks for max number of workers. Spawns a new worker if possible.
%%
-spec spawn_one_worker(#ejm{}, #pool{}) -> {reference() | error, #pool{}}.

spawn_one_worker(St, #pool{max_workers = Max, workers = Workers} = Pool) ->
    Len = length(Workers),
    if  Len < Max ->
            real_spawn_one_worker(St, Pool);
        true ->
            {error, Pool}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc Spawns a new worker, stores its pid (and a ref) in a list,
%% returns the modified pool.
%% @since 2011-07-21 18:00
%%
-spec real_spawn_one_worker(#ejm{}, #pool{}) -> {reference() | error, #pool{}}.

real_spawn_one_worker(C, Pool) ->
    Id = make_ref(),
    Child_config = make_child_config(Pool, Id),
    StartFunc = {ejobman_long_worker, start_link, [Child_config]},
    % for 'permanent' restart policy either worker or handler must contact
    % one another so handler keeps actual list of children. Or use gproc...
    % In the case of 'temporary' the handler does all the housekeeping
    Child = {Id, StartFunc, temporary, 1000, worker, [ejobman_long_worker]},
    Workers = Pool#pool.workers,
    Res = supervisor:start_child(ejobman_long_supervisor, Child),
    mpln_p_debug:pr({?MODULE, 'real_spawn_one_worker res', ?LINE, Res},
        C#ejm.debug, run, 3),
    case Res of
        {ok, Pid} ->
            Ch = #chi{pid=Pid, id=Id, start=now()},
            {Id, Pool#pool{workers = [Ch | Workers]}};
        {ok, Pid, _Info} ->
            Ch = #chi{pid=Pid, id=Id, start=now()},
            {Id, Pool#pool{workers = [Ch | Workers]}};
        {error, _Reason} ->
            {error, Pool}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc creates config (proplist actually) for a child
%% @since 2011-07-21 18:00
%%
-spec make_child_config(#pool{}, reference()) -> list().

make_child_config(Pool, Ref) ->
    [{id, Ref} | Pool#pool.worker_config]
.
%%-----------------------------------------------------------------------------
