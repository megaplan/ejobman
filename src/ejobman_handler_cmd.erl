%%%
%%% ejobman_handler_cmd: received command handling
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
%%% @doc functions that do real handling of the command received by
%%% ejobman_handler
%%%

-module(ejobman_handler_cmd).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([do_command/3, do_short_commands/1, do_worker_cmd/4]).
-export([add_pool/2]).
-export([all_pools_long_command/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").
-include("job.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc converts proplist into a pool record. Adds the pool into worker pools
%% @since 2011-08-03 14:32
%%
-spec add_pool(#ejm{}, list()) -> #ejm{}.

add_pool(#ejm{w_pools = Pools} = St, List) ->
    Pool = ejobman_conf:fill_one_pool_config(List),
    St#ejm{w_pools = [Pool | Pools]}.

%%-----------------------------------------------------------------------------
%%
%% @doc stores the command into a queue and goes to command processing
%% @since 2011-07-15 10:00
%%
-spec do_command(#ejm{}, any(), #job{}) -> #ejm{}.

do_command(St, From, Job) ->
    St_q = store_in_ch_queue(St, From, Job),
    do_short_commands(St_q).

%%-----------------------------------------------------------------------------
%%
%% @doc repeatedly calls for creating new child until either limit
%% reached or command queue exhausted. Returns updated state.
%% @since 2011-07-22 14:54
%%
-spec do_short_commands(#ejm{}) -> #ejm{}.

do_short_commands(#ejm{ch_queue = Q, ch_data = Ch, max_children = Max} = St) ->
    Len = length(Ch),
    mpln_p_debug:pr({?MODULE, 'do_short_commands', ?LINE, Len, Max},
        St#ejm.debug, run, 4),
    case queue:is_empty(Q) of
        false when Len < Max ->
            New_st = check_one_command(St),
            do_short_commands(New_st); % repeat to do commands
        false ->
            mpln_p_debug:pr({?MODULE, 'do_short_commands too many children',
                ?LINE, Len, Max}, St#ejm.debug, run, 3),
            St;
        _ ->
            mpln_p_debug:pr({?MODULE, 'do_short_commands no new child',
                ?LINE, Len, Max}, St#ejm.debug, run, 4),
            St
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc chooses pool and goes on to proceed the job in the selected pool
%% @since 2011-07-22 14:54
%%
-spec do_worker_cmd(#ejm{}, any(), binary(), binary()) -> #ejm{}.

do_worker_cmd(St, From, Method, Url) ->
    Pool = select_pool(St),
    Upd_pool = do_pool_worker_cmd(St, Pool, From, Method, Url),
    store_pool(St, Upd_pool).

%%-----------------------------------------------------------------------------
%%
%% @doc checks for reaching the configured maximum number of children.
%% Proceeds to command if there is a space for a new child. Otherwise
%% stores the command into a queue for later processing.
%% @since 2011-08-02 18:34
%%
-spec do_pool_worker_cmd(#ejm{}, #pool{}, any(), binary(), binary()) -> #pool{}.

do_pool_worker_cmd(St, Pool, From, Method, Url) ->
    Upd_pool = store_in_w_queue(Pool, From, Method, Url),
    do_long_commands(St, Upd_pool).

%%-----------------------------------------------------------------------------
%%
%% @doc calls do_long_commands for every pool
%% @since 2011-08-03 12:19
%%
-spec all_pools_long_command(#ejm{}) -> #ejm{}.

all_pools_long_command(#ejm{w_pools = Pools} = St) ->
    New_pools = lists:map(fun(X) -> do_long_commands(St, X) end, Pools),
    St#ejm{w_pools = New_pools}.

%%-----------------------------------------------------------------------------
%%
%% @doc calls for creating a new worker and assigning a job to it.
%% Returns updated pool.
%% @since 2011-07-22 18:26
%%
-spec do_long_commands(#ejm{}, #pool{}) -> #pool{}.

do_long_commands(St, #pool{w_queue=Q} = Pool) ->
    mpln_p_debug:pr({?MODULE, 'do_long_commands', ?LINE},
        St#ejm.debug, run, 4),
    case queue:is_empty(Q) of
        false ->
            check_one_long_command(St, Pool);
            % no [tail] recursion here because we spawn just one worker
            % at a time. Mass spawning might be time consuming
        _ ->
            mpln_p_debug:pr({?MODULE, 'do_long_commands empty queue',
                ?LINE}, St#ejm.debug, run, 4),
            Pool
    end.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
%%
%% @doc stores the command into a workers queue for later processing.
%% @since 2011-07-22 18:00
%%
-spec store_in_w_queue(#pool{}, any(), binary(), binary()) -> #pool{}.

store_in_w_queue(#pool{w_queue = Q} = Pool, From, Method, Url) ->
    New = queue:in({From, Method, Url}, Q),
    Pool#pool{w_queue=New}.

%%-----------------------------------------------------------------------------
%%
%% @doc stores the command into a queue for later processing.
%% @since 2011-07-22 10:00
%%
-spec store_in_ch_queue(#ejm{}, any(), #job{}) -> #ejm{}.

store_in_ch_queue(#ejm{ch_queue = Q} = St, From, Job) ->
    New = queue:in({From, Job}, Q),
    St#ejm{ch_queue=New}.

%%-----------------------------------------------------------------------------
%%
%% @doc checks for a command in the queue, goes to do_one_long_command
%% to process the command if any.
%% @since 2011-07-22 15:00
%%
-spec check_one_long_command(#ejm{}, #pool{}) -> #pool{}.

check_one_long_command(St, #pool{w_queue = Q} = Pool) ->
    mpln_p_debug:pr({?MODULE, 'check_one_long_command', ?LINE},
        St#ejm.debug, run, 4),
    case queue:out(Q) of
        {{value, Item}, Q2} ->
            do_one_long_command(St, Pool#pool{w_queue=Q2}, Item);
        _ ->
            Pool
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc calls for spawning a worker, assigns a job to the worker.
%% @since 2011-07-22 19:56
%%
-spec do_one_long_command(#ejm{}, #pool{}, tuple()) -> #pool{}.

do_one_long_command(St, Pool, Item) ->
    {Ref, New_pool} = ejobman_worker_spawn:spawn_one_worker(St, Pool),
    case Ref of
        error ->
            mpln_p_debug:pr({?MODULE, 'do_one_long_command error', ?LINE},
                St#ejm.debug, run, 2),
            ok;
        _ ->
            Pid = find_pid(New_pool, Ref),
            % TODO: use catch and/or timeout
            ejobman_long_worker:cmd(Pid, Item)
    end,
    New_pool.

%%-----------------------------------------------------------------------------
%%
%% @doc given a ref finds a pid for the spawned worker
%%
-spec find_pid(#pool{}, reference()) -> pid() | not_found.

find_pid(#pool{workers = Workers}, Ref) ->
    F = fun(#chi{id=Id}) when Id == Ref ->
            true;
        (_) ->
            false
    end,
    case lists:filter(F, Workers) of
        [#chi{pid=Pid} | _] ->
            Pid;
        _ ->
            not_found
    end
.
%%-----------------------------------------------------------------------------
%%
%% @doc checks for a queued command and calls do_one_command for processing
%% if any. Otherwise returns old state.
%% @since 2011-07-22 10:00
%%
-spec check_one_command(#ejm{}) -> #ejm{}.

check_one_command(#ejm{ch_queue = Q} = St) ->
    mpln_p_debug:pr({?MODULE, 'check_one_command', ?LINE},
        St#ejm.debug, run, 4),
    case queue:out(Q) of
        {{value, Item}, Q2} ->
            do_one_command(St#ejm{ch_queue=Q2}, Item);
        _ ->
            St
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc does command processing in background then sends reply to the client.
%% Returns a state with a new child if the one is created.
%% @since 2011-07-15 10:00
%%
-spec do_one_command(#ejm{}, {any(), #job{}}) -> #ejm{}.

do_one_command(St, {From, J}) ->
    mpln_p_debug:pr({?MODULE, 'do_one_command cmd', ?LINE, From, J},
        St#ejm.debug, run, 4),
    % parameters for ejobman_child
    Child_params = [
        {url_rewrite, St#ejm.url_rewrite},
        {from, From},
        {method, J#job.method},
        {url, J#job.url},
        {host, J#job.host},
        {params, J#job.params},
        {auth, J#job.auth},
        {debug, St#ejm.debug}
        ],
    Res = supervisor:start_child(ejobman_child_supervisor, [Child_params]),
    mpln_p_debug:pr({?MODULE, 'do_one_command res', ?LINE, Res},
        St#ejm.debug, run, 4),
    case Res of
        {ok, Pid} ->
            add_child(St, Pid);
        _ ->
            St
    end.

%%-----------------------------------------------------------------------------
%% @doc adds child's pid to the list for later use
%% (e.g.: assign a job, kill, rip, etc...)
-spec add_child(#ejm{}, pid()) -> #ejm{}.

add_child(#ejm{ch_data=Children} = St, Pid) ->
    Ch = #chi{pid = Pid, start = now()},
    St#ejm{ch_data = [Ch | Children]}
.
%%-----------------------------------------------------------------------------
%%
%% @doc randomly selects pool
%%
select_pool(#ejm{w_pools = Pools} = St) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    case catch length(Pools) of
        N when is_integer(N), N > 0 ->
            R = random:uniform(N),
            lists:nth(R, Pools);
        _ ->
            % FIXME: do something here
            mpln_p_debug:pr({?MODULE, 'select_pool error',
                ?LINE, Pools}, St#ejm.debug, run, 0),
            % TODO: add pool on the fly
            erlang:error(dont_f__k_with_us)
    end.
%%-----------------------------------------------------------------------------
%%
%% @doc stores pool in the state
%%
-spec store_pool(#ejm{}, #pool{}) -> #ejm{}.

store_pool(#ejm{w_pools = Pools} = St, P) ->
    Rest = lists:filter(fun(X) -> not_that_pool(X, P#pool.id) end, Pools),
    St#ejm{w_pools = [P | Rest]}
.
%%-----------------------------------------------------------------------------
not_that_pool(Pool, Id) ->
    not is_that_pool(Pool, Id).

%%-----------------------------------------------------------------------------
is_that_pool(#pool{id = X}, Id) when X == Id ->
    true;
is_that_pool(_Pool, _Id) ->
    false.
%%-----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).

make_test_req() ->
    make_test_req(1).

make_test_req(N) ->
    Pid = self(),
    From = {Pid, 'tag'},
    Method = "head",
    Url = "http://localhost:8182/?page" ++ pid_to_list(Pid) ++
        integer_to_list(N),
    {From, Method, Url}.

make_test_st({Pid, _}) ->
    #ejm{
        ch_data = [#chi{pid=Pid, start=now()}],
        max_children = 1,
        debug = [{run, -1}],
        ch_queue = queue:new()
    }.

make_test_data() ->
    {From, Method, Url} = make_test_req(),
    St = make_test_st(From),
    {St, From, Method, Url}
.

do_command_test() ->
    {St, From, Method, Url} = make_test_data(),
    New = do_command(St, From, #job{method = Method, url = Url}),
    Stq = St#ejm{
        ch_queue = queue:in(
            {From, #job{method = Method, url = Url}},
            queue:new())
            },
    ?assert(Stq =:= New).

do_command2_test() ->
    {St, _From, _Method, _Url} = make_test_data(),
    {F2, M2, U2} = make_test_req(2),
    {F3, M3, U3} = make_test_req(3),
    {F4, M4, U4} = make_test_req(4),
    {F5, M5, U5} = make_test_req(5),
    St2 = store_in_ch_queue(St , F2, #job{method = M2, url = U2}),
    St3 = store_in_ch_queue(St2, F3, #job{method = M3, url = U3}),
    St4 = store_in_ch_queue(St3, F4, #job{method = M4, url = U4}),
    St5 = store_in_ch_queue(St4, F5, #job{method = M5, url = U5}),
    mpln_p_debug:pr({?MODULE, 'do_command2_test', ?LINE, St5}, [], run, 0),
    Res = do_short_commands(St5),
    mpln_p_debug:pr({?MODULE, 'do_command2_test res', ?LINE, Res}, [], run, 0),
    ok.

-endif.
%%-----------------------------------------------------------------------------
