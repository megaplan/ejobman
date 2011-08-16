%%%
%%% ejobman_handler: gen_server that handles messages from ejobman_receiver
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
%%% @since 2011-07-15 10:00
%%% @license MIT
%%% @doc a gen_server that gets messages from ejobman_receiver and calls
%%% ejobman_child_supervisor to spawn a new child to do all the dirty work
%%%

-module(ejobman_handler).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([cmd/1, remove_child/1]).
-export([cmd2/2, cmd2/3]).
-export([add_pool/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").
-include("job.hrl").
-include("amqp_client.hrl").

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

%-define(GRP, ejobman_handler_workers).

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(Config) ->
    application:start(inets),
    application:start(ssl),
    C = ejobman_conf:get_config_hdl(Config),
    New = prepare_workers(C),
    % trap_exit is unnecessary. Children are ripped by supervisor
    %process_flag(trap_exit, true),
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE}, C#ejm.debug, run, 1),
    {ok, New, ?T}.
%%-----------------------------------------------------------------------------
%%
%% Handling call messages
%% @since 2011-07-15 11:00
%%
-spec handle_call(any(), any(), #ejm{}) ->
    {noreply, #ejm{}, non_neg_integer()}
    | {any(), any(), #ejm{}, non_neg_integer()}.

%% @doc deletes disposable child from the state
handle_call({remove_child, Pid}, _From, St) ->
    St_d = do_smth(St),
    New = remove_child(St_d, Pid),
    {reply, ok, New, ?T};

%% @doc calls disposable child
handle_call({cmd, Job}, From, St) ->
    St_d = do_smth(St),
    New = ejobman_handler_cmd:do_command(St_d, From, Job),
    {noreply, New, ?T};

%% @doc calls long-lasting worker
handle_call({cmd2, Method, Url}, From, St) ->
    St_d = do_smth(St),
    New = ejobman_handler_cmd:do_worker_cmd(St_d, From, Method, Url),
    {noreply, New, ?T};

%% @doc adds a new pool
handle_call({add_pool, Pool}, _From, St) ->
    St_d = do_smth(St),
    New = ejobman_handler_cmd:add_pool(St_d, Pool),
    {reply, ok, New, ?T};

handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call(status, _From, St) ->
    {reply, St, St, ?T};
handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'other', ?LINE, _N}, St#ejm.debug, run, 3),
    New = do_smth(St),
    {reply, {error, unknown_request}, New, ?T}.
%%-----------------------------------------------------------------------------
%%
%% Handling cast messages
%% @since 2011-07-15 11:00
%%
-spec handle_cast(any(), #ejm{}) -> any().

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(st0p, St) ->
    St;
handle_cast({add_worker, Pool_id}, St) ->
    mpln_p_debug:pr({?MODULE, 'cast add_worker', ?LINE, Pool_id},
        St#ejm.debug, run, 4),
    St_w = throw_worker_pools(St, Pool_id),
    New = do_smth(St_w),
    {noreply, New, ?T};
handle_cast(_N, St) ->
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _N}, St#ejm.debug, run, 3),
    New = do_smth(St),
    {noreply, New, ?T}.
%%-----------------------------------------------------------------------------
terminate(_, State) ->
    remove_workers(State),
    mpln_p_debug:pr({?MODULE, 'terminate', ?LINE}, State#ejm.debug, run, 1),
    ok.
%%-----------------------------------------------------------------------------
%%
%% Handling all non call/cast messages
%% @since 2011-07-15 11:00
%%
-spec handle_info(any(), #ejm{}) -> {noreply, #ejm{}, non_neg_integer()}.

handle_info({'DOWN', Mref, _, Oid, _Info}=Msg, State) ->
    mpln_p_debug:pr({?MODULE, info_down, ?LINE, Msg}, State#ejm.debug, run, 2),
    St_w = handle_crashed(State, Mref, Oid),
    New = do_smth(St_w),
    {noreply, New, ?T};
handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, info_timeout, ?LINE}, State#ejm.debug, run, 6),
    New = do_smth(State),
    {noreply, New, ?T};
handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, other, ?LINE, _Req}, State#ejm.debug, run, 3),
    New = do_smth(State),
    {noreply, New, ?T}.
%%-----------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
-spec start() -> any().
%%
%% @doc starts handler gen_server
%% @since 2011-07-15 11:00
%%
start() ->
    start_link().

%%-----------------------------------------------------------------------------
-spec start_link() -> any().
%%
%% @doc starts handler gen_server with pre-defined config
%% @since 2011-07-15 11:00
%%
start_link() ->
    start_link(?CONF).

-spec start_link(string()) -> any().
%%
%% @doc starts handler gen_server with given config
%% @since 2011-07-15 11:00
%%
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%-----------------------------------------------------------------------------
-spec stop() -> any().
%%
%% @doc stops handler gen_server
%% @since 2011-07-15 11:00
%%
stop() ->
    gen_server:call(?MODULE, stop).

%%-----------------------------------------------------------------------------
%%
%% @doc calls any received command to be executed by long-lasting worker
%% @since 2011-07-22 12:00
%%
-spec cmd2(binary(), binary()) -> ok.

cmd2(Method, Url) ->
    cmd2(Method, Url, 5000).

%%
%% @doc calls any received command to be executed by long-lasting worker
%% with timeout defined
%% @since 2011-07-22 12:00
%%
-spec cmd2(binary(), binary(), non_neg_integer()) -> ok.

cmd2(Method, Url, Timeout) ->
    gen_server:call(?MODULE, {cmd2, Method, Url}, Timeout).

%%-----------------------------------------------------------------------------
%%
%% @doc calls any received command to be executed by disposable child
%% @since 2011-07-15 11:00
%%
-spec cmd(#job{}) -> ok.

cmd(Job) ->
    gen_server:call(?MODULE, {cmd, Job}).

%%-----------------------------------------------------------------------------
%%
%% @doc asks ejobman_handler to remove child from the list
%%
-spec remove_child(pid()) -> ok.

remove_child(Pid) ->
    gen_server:cast(?MODULE, {remove_child, Pid}).

%%-----------------------------------------------------------------------------
%%
%% @doc adds a new pool into the server state. Input is a proplist of
%% {key, value} tuples.
%% @since 2011-08-03 15:04
%%
-spec add_pool(list()) -> ok.

add_pool(List) ->
    gen_server:call(?MODULE, {add_pool, List}).

%%-----------------------------------------------------------------------------
%%
%% @doc cast to add a new worker to the pool
%% @since 2011-08-16 13:19
%%
-spec add_worker(any()) -> ok.

add_worker(Pool_id) ->
    gen_server:cast(?MODULE, {add_worker, Pool_id}).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc removes child from the list of children
%%
-spec remove_child(#ejm{}, pid()) -> #ejm{}.

remove_child(#ejm{ch_data=Ch} = St, Pid) ->
    F = fun(#chi{pid=X}) when X == Pid ->
            false;
        (_) ->
            true
    end,
    New = lists:filter(F, Ch),
    St#ejm{ch_data=New}.

%%-----------------------------------------------------------------------------
%%
%% @doc calls handle_crashed_pid for every pool
%%
-spec handle_crashed(#ejm{}, reference(), any()) -> #ejm{}.

handle_crashed(#ejm{w_pools=Pools} = State, Mref, Obj) ->
    New_pools = lists:map(fun(X) -> handle_crashed_pid(State, X, Mref, Obj) end,
        Pools),
    State#ejm{w_pools = New_pools}.

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
    mpln_p_debug:pr({?MODULE, handle_crashed_pid, ?LINE,
        Found, Not_found, Wait_add}, St#ejm.debug, run, 5),
    Pool#pool{waiting = Wait_add ++ Waiting, workers=Not_found}.

%%-----------------------------------------------------------------------------
-spec crashed_pid_action(#pool{}, any(), list()) -> list().

crashed_pid_action(Pool, Obj, Acc) ->
    case check_pool_restart_policy(Pool) of
        delay ->
            Now = now(),
            [{Obj, Now} | Acc];
        restart ->
            add_worker(Pool#pool.id),
            Acc;
        _ ->
            Acc
    end.

%%-----------------------------------------------------------------------------
check_pool_restart_policy(#pool{restart_policy='delay'}) ->
    'delay';
check_pool_restart_policy(#pool{restart_policy='restart'}) ->
    'restart';
check_pool_restart_policy(_) ->
    'none'.

%%-----------------------------------------------------------------------------
%%
%% @doc does miscellaneous periodic checks. E.g.: check for children. Returns
%% updated state.
%%
-spec do_smth(#ejm{}) -> #ejm{}.

do_smth(State) ->
    mpln_p_debug:pr({?MODULE, 'do_smth', ?LINE}, State#ejm.debug, run, 5),
    Stw = check_workers(State),
    Stc = check_children(Stw),
    check_queued_commands(Stc).

%%-----------------------------------------------------------------------------
%%
%% @doc calls to process all the queued commands
%%
-spec check_queued_commands(#ejm{}) -> #ejm{}.

check_queued_commands(St) ->
    St_short = ejobman_handler_cmd:do_short_commands(St),
    ejobman_handler_cmd:all_pools_long_command(St_short).

%%-----------------------------------------------------------------------------
%%
%% @doc checks for old workers and live workers, adds workers if necessary
%%
-spec check_workers(#ejm{}) -> #ejm{}.

check_workers(St) ->
    Stw = clear_waiting_workers(St),
    Sto = check_old_workers(Stw),
    Stl = check_live_workers(Sto),
    replenish_worker_pools(Stl).

%%-----------------------------------------------------------------------------
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
                Delta =< Limit * 1000
    end,
    {Found, Not_found} = lists:partition(F, Waiting),
    lists:foreach(fun(_) -> add_worker(Pool#pool.id) end, Not_found),
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
%% @doc stops old disengaged workers (?), stops any too old workers.
%% Returns the updated pool with quite young workers only
%%
-spec check_pool_old_workers(#ejm{}, #pool{}) -> #pool{}.

check_pool_old_workers(St, Pool) ->
    {Ok, Old} = separate_workers(Pool),
    mpln_p_debug:pr({?MODULE, 'check_pool_old_workers', ?LINE,
        Pool#pool.id, Ok, Old}, St#ejm.debug, run, 5),
    terminate_old_workers(Old),
    Pool#pool{workers=Ok}
.
%%-----------------------------------------------------------------------------
%%
%% @doc spawns the necessary amount of workers for every pool
%%
replenish_worker_pools(#ejm{w_pools = Pools} = St) ->
    New_pools = lists:map(fun(X) -> replenish_one_pool(St, X) end, Pools),
    St#ejm{w_pools = New_pools}
.
%%-----------------------------------------------------------------------------
%%
%% @doc spawns the necessary amount of workers for the pool
%%
-spec replenish_one_pool(#ejm{}, #pool{}) -> #pool{}.

replenish_one_pool(St, #pool{min_workers=Min, workers=Workers, waiting=Waiting}
        = Pool) ->
    Delta = Min - length(Workers) - length(Waiting),
    if  Delta > 0 ->
            spawn_n_workers(St, Pool, Delta);
        true ->
            Pool
    end.

%%-----------------------------------------------------------------------------
throw_worker_pools(#ejm{w_pools = Pools} = St, Id) ->
    New_pools = lists:map(fun(X) -> throw_worker_one_pool(St, X, Id) end,
        Pools),
    St#ejm{w_pools = New_pools}
.

%%-----------------------------------------------------------------------------
%%
%% @doc adds a worker for the appropriate pool if there is space for it
%%
throw_worker_one_pool(St, #pool{id=X, min_workers=Min, workers=Workers,
        waiting=Waiting} = Pool, Id) when X =:= Id ->
    Delta = Min - length(Workers) - length(Waiting),
    if  Delta > 0 ->
            spawn_n_workers(St, Pool, 1);
        true ->
            Pool
    end;
throw_worker_one_pool(_St, Pool, _) ->
    Pool.

%%-----------------------------------------------------------------------------
%%
%% @doc checks for live workers in pools
%%
check_live_workers(#ejm{w_pools = Pools} = St) ->
    New_pools = lists:map(fun check_pool_live_workers/1, Pools),
    St#ejm{w_pools = New_pools}
.
%%-----------------------------------------------------------------------------
%%
%% @doc checks for live workers in a pool.
%%
-spec check_pool_live_workers(#pool{}) -> #pool{}.

check_pool_live_workers(#pool{workers=Workers} = Pool) ->
    F = fun(X) ->
        case process_info(X#chi.pid) of
            undefined -> false;
            _ -> true
        end
    end,
    New = lists:filter(F, Workers),
    Pool#pool{workers=New}
.
%%-----------------------------------------------------------------------------
%%
%% @doc for each worker in a list calls supervisor to stop the worker
%%
-spec terminate_old_workers([#chi{}]) -> any().

terminate_old_workers(List) ->
    lists:foreach(fun terminate_one_worker/1, List)
.
%%-----------------------------------------------------------------------------
%%
%% @doc separate workers on their working time. Returns lists of normal
%% workers and workers that need to be terminated
%%
-spec separate_workers(#pool{}) -> {list(), list()}.

separate_workers(#pool{w_duration=Limit, workers=Workers}) ->
    Now = now(),
    F = fun(#chi{start = T}) ->
        Delta = timer:now_diff(Now, T),
        Delta < Limit * 1000
    end,
    lists:partition(F, Workers).

%%-----------------------------------------------------------------------------
%%
%% @doc checks that all the children are alive. Returns new state with
%% live children only
%%
-spec check_children(#ejm{}) -> #ejm{}.

check_children(#ejm{ch_data=Ch} = State) ->
    New = lists:filter(fun check_child/1, Ch),
    State#ejm{ch_data = New}.

%%-----------------------------------------------------------------------------
%%
%% @doc checks whether the given child does something
%%
-spec check_child(#chi{}) -> boolean().

check_child(#chi{pid=Pid}) ->
    case process_info(Pid, reductions) of
        {reductions, _N} ->
            true;
        _ ->
            false
    end.

%%-----------------------------------------------------------------------------
-spec prepare_workers(#ejm{}) -> #ejm{}.
%%
%% @doc spawns the configured minimum of long-lasting workers
%%
prepare_workers(#ejm{w_pools = Pools} = C) ->
    New_pools = lists:map(fun(X) -> spawn_workers(C, X) end, Pools),
    C#ejm{w_pools = New_pools}.

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
-spec remove_workers(#ejm{}) -> ok.
%%
%% @doc terminates all the workers in all the pools
%%
remove_workers(#ejm{w_pools = Pools}) ->
    lists:foreach(fun terminate_workers/1, Pools).

%%-----------------------------------------------------------------------------
-spec terminate_workers(#pool{}) -> ok.
%%
%% @doc terminates all the workers
%%
terminate_workers(#pool{workers = Workers}) ->
    lists:foreach(fun terminate_one_worker/1, Workers).

%%-----------------------------------------------------------------------------
-spec terminate_one_worker(#chi{}) -> any().
%%
%% @doc terminates one worker
%%
terminate_one_worker(#chi{id=Id, mon=Mref}) ->
    % we don't need to monitor workers we stop manually
    erlang:demonitor(Mref),
    supervisor:terminate_child(ejobman_long_supervisor, Id),
    supervisor:delete_child(ejobman_long_supervisor, Id).

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
remove_child_test() ->
    Pid = self(),
    Me = #chi{pid=Pid, start=now()},
    Ch = make_fake_children(),
    St = #ejm{ch_data = [Me | Ch]},
    ?assert(#ejm{ch_data=Ch} =:= remove_child(St, Pid)).

check_mix_children_test() ->
    Me = #chi{pid=self(), start=now()},
    Ch = make_fake_children(),
    St = #ejm{ch_data = [Me | Ch]},
    ?assert(#ejm{ch_data=[Me]} =:= check_children(St)).
 
check_fake_children_test() ->
    Ch = make_fake_children(),
    St = #ejm{ch_data = Ch},
    ?assert(#ejm{ch_data=[]} =:= check_children(St)).
 
make_fake_children() ->
    L = [
        "<0.12340.5678>",
        "<0.32767.7136>",
        "<0.7575.5433>"
    ],
    lists:map(fun(X) -> #chi{pid=list_to_pid(X), start=now()} end, L).
-endif.
%%-----------------------------------------------------------------------------
