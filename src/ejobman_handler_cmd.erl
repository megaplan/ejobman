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

-export([do_command/4, do_short_commands/1, do_worker_cmd/4]).
-export([do_long_commands/1]).

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
%% @doc stores the command into a queue and goes to command processing
%% @since 2011-07-15 10:00
%%
-spec do_command(#ejm{}, any(), binary(), binary()) -> #ejm{}.

do_command(St, From, Method, Url) ->
    St_q = store_in_ch_queue(St, From, Method, Url),
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
%% @doc checks for reaching the configured maximum number of children.
%% Proceeds to command if there is a space for a new child. Otherwise
%% stores the command into a queue for later processing.
%% @since 2011-07-22 14:54
%%
-spec do_worker_cmd(#ejm{}, any(), binary(), binary()) -> #ejm{}.

do_worker_cmd(St, From, Method, Url) ->
    St_q = store_in_w_queue(St, From, Method, Url),
    do_long_commands(St_q).
%%-----------------------------------------------------------------------------
%%
%% @doc calls for creating a new worker and assigning a job to it.
%% Returns updated state.
%% @since 2011-07-22 18:26
%%
-spec do_long_commands(#ejm{}) -> #ejm{}.

do_long_commands(#ejm{w_queue=Q, workers=Workers, max_workers=Max} = St) ->
    Len = length(Workers),
    mpln_p_debug:pr({?MODULE, 'do_long_commands', ?LINE, Len, Max},
        St#ejm.debug, run, 4),
    case queue:is_empty(Q) of
        false when Len < Max ->
            New_st = check_one_long_command(St),
            % don't spawn all the heavy workers immediately
            % do_long_commands(New_st);
            New_st;
        false ->
            mpln_p_debug:pr({?MODULE, 'do_long_commands too many workers',
                ?LINE, Len, Max}, St#ejm.debug, run, 3),
            St;
        _ ->
            mpln_p_debug:pr({?MODULE, 'do_long_commands no new worker',
                ?LINE, Len, Max}, St#ejm.debug, run, 4),
            St
    end.
%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
%%
%% @doc stores the command into a workers queue for later processing.
%% @since 2011-07-22 18:00
%%
-spec store_in_w_queue(#ejm{}, any(), binary(), binary()) -> #ejm{}.

store_in_w_queue(#ejm{w_queue = Q} = St, From, Method, Url) ->
    New = queue:in({From, Method, Url}, Q),
    St#ejm{w_queue=New}.
%%-----------------------------------------------------------------------------
%%
%% @doc stores the command into a queue for later processing.
%% @since 2011-07-22 10:00
%%
-spec store_in_ch_queue(#ejm{}, any(), binary(), binary()) -> #ejm{}.

store_in_ch_queue(#ejm{ch_queue = Q} = St, From, Method, Url) ->
    New = queue:in({From, Method, Url}, Q),
    St#ejm{ch_queue=New}.
%%-----------------------------------------------------------------------------
%%
%% @doc checks for a command in the queue, goes to do_one_long_command
%% to process the command if any.
%% @since 2011-07-22 15:00
%%
-spec check_one_long_command(#ejm{}) -> #ejm{}.

check_one_long_command(#ejm{w_queue = Q} = St) ->
    mpln_p_debug:pr({?MODULE, 'check_one_long_command', ?LINE},
        St#ejm.debug, run, 4),
    case queue:out(Q) of
        {{value, Item}, Q2} ->
            do_one_long_command(St#ejm{w_queue=Q2}, Item);
        _ ->
            St
    end.
%%-----------------------------------------------------------------------------
%%
%% @doc calls for spawning a worker, assigns a job to the worker.
%% @since 2011-07-22 19:56
%%
-spec do_one_long_command(#ejm{}, tuple()) -> #ejm{}.

do_one_long_command(St, Item) ->
    {Ref, Stw} = ejobman_worker_spawn:spawn_one_worker(St),
    case Ref of
        error ->
            mpln_p_debug:pr({?MODULE, 'do_one_long_command error', ?LINE},
                St#ejm.debug, run, 2),
            ok;
        _ ->
            Pid = find_pid(Stw, Ref),
            ejobman_long_worker:cmd(Pid, Item)
    end,
    Stw.
%%-----------------------------------------------------------------------------
%%
%% @doc given a ref finds a pid for spawned worker
%%
-spec find_pid(#ejm{}, reference()) -> pid() | not_found.

find_pid(#ejm{workers = Workers}, Ref) ->
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
%% @doc do command processing in background then send reply to the client.
%% Returns a state with a new child if the one is created.
%% @since 2011-07-15 10:00
%%
-spec do_one_command(#ejm{}, tuple()) -> #ejm{}.

do_one_command(St, {From, Method, Url}) ->
    mpln_p_debug:pr({?MODULE, 'do_one_command cmd', ?LINE, From, Method, Url},
        St#ejm.debug, run, 4),
    % parameters for ejobman_child
    Params = [
        {from, From},
        {method, Method},
        {url, Url},
        {debug, St#ejm.debug}
        ],
    Res = supervisor:start_child(ejobman_child_supervisor, [Params]),
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
%%%----------------------------------------------------------------------------
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
    New = do_command(St, From, Method, Url),
    Stq = St#ejm{ch_queue = queue:in({From, Method, Url}, queue:new())},
    ?assert(Stq =:= New).

do_command2_test() ->
    {St, _From, _Method, _Url} = make_test_data(),
    {F2, M2, U2} = make_test_req(2),
    {F3, M3, U3} = make_test_req(3),
    {F4, M4, U4} = make_test_req(4),
    {F5, M5, U5} = make_test_req(5),
    St2 = store_in_ch_queue(St, F2, M2, U2),
    St3 = store_in_ch_queue(St2, F3, M3, U3),
    St4 = store_in_ch_queue(St3, F4, M4, U4),
    St5 = store_in_ch_queue(St4, F5, M5, U5),
    mpln_p_debug:pr({?MODULE, 'do_command2_test', ?LINE, St5}, [], run, 0),
    Res = do_short_commands(St5),
    mpln_p_debug:pr({?MODULE, 'do_command2_test res', ?LINE, Res}, [], run, 0),
    ok.

-endif.
%%-----------------------------------------------------------------------------
