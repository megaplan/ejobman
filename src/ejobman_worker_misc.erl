%%%
%%% ejobman_worker_misc: miscellaneous functions for worker pools
%%%
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author arkdro <arkdro@gmail.com>
%%% @since 2011-08-16 16:51
%%% @license MIT
%%% @doc miscellaneous functions for worker pools
%%%

-module(ejobman_worker_misc).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([check_workers/1, prepare_workers/1, throw_worker_pools/2]).
-export([remove_workers/1, handle_crashed/3, get_process_info/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc gets pools and processes info
%% @since 2011-08-16 16:51
%%
-spec get_process_info(#ejm{}) -> any().

get_process_info(#ejm{w_pools = Pools}) ->
    lists:map(fun get_process_one_pool/1, Pools).

%%-----------------------------------------------------------------------------
%%
%% @doc checks for old workers and live workers, adds workers if necessary
%% @since 2011-08-16 16:51
%%
-spec check_workers(#ejm{}) -> #ejm{}.

check_workers(St) ->
    Stw = clear_waiting_workers(St),
    Sto = check_old_workers(Stw),
    fetch_worker_os_pids(Sto).

%%-----------------------------------------------------------------------------
%%
%% @doc spawns the configured minimum of long-lasting workers
%% @since 2011-08-16 16:51
%%
-spec prepare_workers(#ejm{}) -> #ejm{}.

prepare_workers(#ejm{w_pools = Pools} = C) ->
    New_pools = lists:map(fun(X) -> spawn_workers(C, X) end, Pools),
    C#ejm{w_pools = New_pools}.

%%-----------------------------------------------------------------------------
%%
%% @doc calls throw_worker_one_pool for every pool
%% @since 2011-08-16 16:51
%%
-spec throw_worker_pools(#ejm{}, any()) -> #ejm{}.

throw_worker_pools(#ejm{w_pools = Pools} = St, Id) ->
    New_pools = lists:map(fun(X) -> throw_worker_one_pool(St, X, Id) end,
        Pools),
    St#ejm{w_pools = New_pools}.

%%-----------------------------------------------------------------------------
%%
%% @doc terminates all the workers in all the pools
%% @since 2011-08-16 16:51
%%
-spec remove_workers(#ejm{}) -> ok.

remove_workers(#ejm{w_pools = Pools}) ->
    lists:foreach(fun terminate_workers/1, Pools).

%%-----------------------------------------------------------------------------
%%
%% @doc calls handle_crashed_pid for every pool
%% @since 2011-08-16 16:51
%%
-spec handle_crashed(#ejm{}, reference(), any()) -> #ejm{}.

handle_crashed(#ejm{w_pools=Pools} = State, Mref, Obj) ->
    New_pools = lists:map(fun(X) -> handle_crashed_pid(State, X, Mref, Obj) end,
        Pools),
    State#ejm{w_pools = New_pools}.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc clears old pids from restart waiting lists, so the new workers may
%% be spawned later.
%%
-spec clear_waiting_workers(#ejm{}) -> #ejm{}.

clear_waiting_workers(#ejm{w_pools = Pools} = St) ->
    New_pools = lists:map(fun(X) -> clear_pool_waiting_workers(St, X) end,
        Pools),
    St#ejm{w_pools = New_pools}.

%%-----------------------------------------------------------------------------
%%
%% @doc clears old pids from the restart waiting list of a pool
%%
-spec clear_pool_waiting_workers(#ejm{}, #pool{}) -> #pool{}.

clear_pool_waiting_workers(St, #pool{restart_delay=Limit, waiting=Waiting} =
        Pool) ->
    Now = now(),
    F = fun ({_Obj, Time}) ->
                Delta = timer:now_diff(Now, Time),
                Delta =< Limit * 1000000
    end,
    {Found, Not_found} = lists:partition(F, Waiting),
    lists:foreach(fun(_) -> ejobman_worker:add_worker(Pool#pool.id) end,
        Not_found),
    mpln_p_debug:pr({?MODULE, 'clear_pool_waiting_workers', ?LINE,
        Pool#pool.id, Found, Not_found}, St#ejm.debug, run, 5),
    Pool#pool{waiting = Found}.

%%-----------------------------------------------------------------------------
%%
%% @doc stops old workers for all the pools.
%% Returns a new state with quite young workers only
%%
-spec check_old_workers(#ejm{}) -> #ejm{}.

check_old_workers(#ejm{w_pools = Pools} = St) ->
    New_pools = lists:map(fun(X) -> check_pool_old_workers(St, X) end, Pools),
    St#ejm{w_pools = New_pools}
.
%%-----------------------------------------------------------------------------
%%
%% @doc restarts too old workers. Returns the updated pool with new workers
%%
-spec check_pool_old_workers(#ejm{}, #pool{}) -> #pool{}.

check_pool_old_workers(St, Pool) ->
    F = fun(X) -> check_restart_old_worker(St, Pool#pool.w_duration, X) end,
    New = lists:map(F, Pool#pool.workers),
    Pool#pool{workers=New}.

%%-----------------------------------------------------------------------------
%%
%% @doc adds a worker for the appropriate pool if there is space for it
%%
throw_worker_one_pool(St, #pool{id=X} = Pool, Id) when X =:= Id ->
    spawn_n_workers(St, Pool, 1);
throw_worker_one_pool(_St, Pool, _) ->
    Pool.

%%-----------------------------------------------------------------------------
%%
%% @doc spawns the configured for a pool a minimum number of workers
%%
-spec spawn_workers(#ejm{}, #pool{}) -> #pool{}.

spawn_workers(C, #pool{min_workers=N} = P) ->
    spawn_n_workers(C, P, N).

%%
%% @doc spawns N workers
%%
-spec spawn_n_workers(#ejm{}, #pool{}, non_neg_integer()) -> #pool{}.

spawn_n_workers(State, Pool, N) ->
    lists:foldl(fun(_X, Pool_in) ->
            {_, New} = ejobman_worker_spawn:spawn_one_worker(State, Pool_in),
            New
        end,
        Pool,
        lists:duplicate(N, true)
    ).

%%-----------------------------------------------------------------------------
%%
%% @doc terminates all the workers
%%
-spec terminate_workers(#pool{}) -> ok.

terminate_workers(#pool{workers = Workers}) ->
    lists:foreach(fun terminate_one_worker/1, Workers).

%%-----------------------------------------------------------------------------
%%
%% @doc terminates one worker
%%
-spec terminate_one_worker(#chi{}) -> any().

terminate_one_worker(#chi{id=Id, mon=Mref}) ->
    % we don't need to monitor workers we stop manually
    erlang:demonitor(Mref),
    supervisor:terminate_child(ejobman_long_supervisor, Id),
    supervisor:delete_child(ejobman_long_supervisor, Id).

%%-----------------------------------------------------------------------------
%%
%% @doc performs pool restart policy based action for the crashed pid
%%
-spec handle_crashed_pid(#ejm{}, #pool{}, reference(), any()) -> #pool{}.

handle_crashed_pid(St, #pool{waiting=Waiting, workers=Workers} =
        Pool, Mref, Obj) ->
    F = fun (#chi{mon=Mon}) when Mon =:= Mref ->
                true;
            (_) ->
                false
    end,
    {Found, Not_found} = lists:partition(F, Workers),
    F2 = fun(_X, Acc) ->
        crashed_pid_action(Pool, Obj, Acc)
    end,
    Wait_add = lists:foldl(F2, [], Found),
    mpln_p_debug:pr({?MODULE, handle_crashed_pid, ?LINE, Pool#pool.id,
        Found, Not_found, Wait_add}, St#ejm.debug, run, 5),
    Pool#pool{waiting = Wait_add ++ Waiting, workers=Not_found}.

%%-----------------------------------------------------------------------------
%%
%% @doc does either of the restart actions: none, cast to immediate restart,
%% store time for later check and delayed restart
%%
-spec crashed_pid_action(#pool{}, any(), list()) -> list().

crashed_pid_action(Pool, Obj, Acc) ->
    case check_pool_restart_policy(Pool) of
        delay ->
            Now = now(),
            [{Obj, Now} | Acc];
        restart ->
            ejobman_worker:add_worker(Pool#pool.id),
            Acc;
        _ ->
            Acc
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc yes, Virginia. It checks pool restart policy
%%
check_pool_restart_policy(#pool{restart_policy='delay'}) ->
    'delay';
check_pool_restart_policy(#pool{restart_policy='restart'}) ->
    'restart';
check_pool_restart_policy(_) ->
    'none'.

%%-----------------------------------------------------------------------------
%%
%% @doc gets process info for one pool
%%
-spec get_process_one_pool(#pool{}) -> any().

get_process_one_pool(#pool{} = P) ->
    [
        {pool_id, P#pool.id},
        {work_duration, P#pool.w_duration},
        {worker_config, expand_worker_config(P#pool.worker_config)},
        {workers, expand_workers(P#pool.workers)},
        {waiting, P#pool.waiting},
        {restart_delay, P#pool.restart_delay},
        {restart_policy, P#pool.restart_policy},
        {queue_len, queue:len(P#pool.w_queue)},
        {min_workers, P#pool.min_workers}
    ]
.
%%-----------------------------------------------------------------------------
expand_worker_config(List) ->
    proplists:get_value(name, List).

%%-----------------------------------------------------------------------------
-spec expand_workers([#chi{}]) -> any().

expand_workers(List) ->
    lists:map(fun expand_one_worker/1, List).

%%-----------------------------------------------------------------------------
-spec expand_one_worker(#chi{}) -> any().

expand_one_worker(W) ->
    {
        W#chi.pid,
        W#chi.os_pid,
        %W#chi.id,
        %W#chi.mon,
        mpln_misc_time:get_time_str(W#chi.start)
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc fetch os_pids for all the pools
%%
-spec fetch_worker_os_pids(#ejm{}) -> #ejm{}.

fetch_worker_os_pids(#ejm{w_pools = Pools} = St) ->
    New_pools = lists:map(fun fetch_pool_os_pids/1, Pools),
    St#ejm{w_pools = New_pools}.

%%-----------------------------------------------------------------------------
%%
%% @doc fetch os_pids (if it has not been done yet) for all the workers
%% in the pool
%%
fetch_pool_os_pids(#pool{workers=Workers} = Pool) ->
    F = fun (#chi{pid=Pid, os_pid=undefined} = C) ->
                Os_pid = ejobman_long_worker:get_os_pid(Pid),
                C#chi{os_pid=Os_pid};
            (C) ->
                C
    end,
    New_workers = lists:map(F, Workers),
    Pool#pool{workers = New_workers}.

%%-----------------------------------------------------------------------------
%%
%% @doc restarts one worker
%%
-spec restart_one_worker(#ejm{}, #chi{}) -> any().

restart_one_worker(St, #chi{id=Id, mon=Mref} = Orig) ->
    erlang:demonitor(Mref),
    Res_t = supervisor:terminate_child(ejobman_long_supervisor, Id),
    Res = supervisor:restart_child(ejobman_long_supervisor, Id),
    mpln_p_debug:pr({?MODULE, 'restart_one_worker', ?LINE, Orig, Res_t, Res},
        St#ejm.debug, run, 3),
    case Res of
        {ok, Pid} ->
            New_mref = erlang:monitor(process, Pid),
            #chi{pid=Pid, id=Id, start=now(), mon=New_mref};
        {ok, Pid, _Info} ->
            New_mref = erlang:monitor(process, Pid),
            #chi{pid=Pid, id=Id, start=now(), mon=New_mref};
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'restart_one_worker error', ?LINE,
                Orig, Res_t, Reason}, St#ejm.debug, run, 0),
            Orig
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc restarts the worker if it is too old
%%
-spec check_restart_old_worker(#ejm{}, non_neg_integer(), #chi{}) -> #chi{}.

check_restart_old_worker(St, Dur, #chi{start=Start} = Worker) ->
    Now = now(),
    Delta = timer:now_diff(Now, Start),
    if  Delta > Dur * 1000000 ->
            restart_one_worker(St, Worker);
        true ->
            Worker
    end.

%%-----------------------------------------------------------------------------
