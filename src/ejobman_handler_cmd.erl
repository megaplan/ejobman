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

-export([do_command/3, do_short_commands/1]).
-export([do_command_result/3]).
-export([remove_child/3]).

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
%% @doc stores the command into a queue and goes to command processing
%% @since 2011-07-15 10:00
%%
-spec do_command(#ejm{}, any(), #job{}) -> #ejm{}.

do_command(St, From, Job) ->
    St_q = store_in_ch_queue(St, From, Job),
    do_short_commands(St_q).

%%-----------------------------------------------------------------------------
%%
%% @doc iterates over all queues for short commands
%%
-spec do_short_commands(#ejm{}) -> #ejm{}.

do_short_commands(#ejm{ch_queues=Data} = St) ->
    F = fun(Gid, _, Acc) ->
        short_command_step(Acc, Gid)
    end,
    St_s = dict:fold(F, St, Data),
    St_s.

%%-----------------------------------------------------------------------------
%%
%% @doc logs a command result to the job log
%% @since 2011-10-19 18:00
%%
-spec do_command_result(#ejm{}, tuple(), reference()) -> ok.

do_command_result(St, Res, Id) ->
    ejobman_log:log_job_result(St, Res, Id).

%%-----------------------------------------------------------------------------
%%
%% @doc removes child from the list of children
%% @since 2011-11-14 17:14
%%
-spec remove_child(#ejm{}, pid(), any()) -> #ejm{}.

remove_child(St, Pid, Group) ->
    Ch = fetch_spawned_children(St, Group),
    F = fun(#chi{pid=X}) when X == Pid ->
            false;
        (_) ->
            true
    end,
    New_ch = lists:filter(F, Ch),
    store_spawned_children(St, Group, New_ch).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc does one iteration for given group over queue and spawned children.
%% Returns updated state with new queue and spawned children
%%
-spec short_command_step(#ejm{}, any()) -> #ejm{}.

short_command_step(#ejm{job_groups=Groups, max_children=Max} = St, Gid) ->
    Q = fetch_job_queue(St, Gid),
    Ch = fetch_spawned_children(St, Gid),
    Max = get_group_max(Groups, Gid, Max),
    {New_q, New_ch} = do_short_command_queue(St, {Q, Ch}, Max),
    St_j = store_job_queue(St, Gid, New_q),
    St_ch = store_spawned_children(St_j, Gid, New_ch),
    St_ch.

%%-----------------------------------------------------------------------------
%%
%% @doc stores a queue for the given group to a dictionary
%%
-spec store_job_queue(#ejm{}, any(), queue()) -> #ejm{}.

store_job_queue(#ejm{ch_queues=Data} = St, Gid, Q) ->
    New_dict = dict:store(Gid, Q, Data),
    St#ejm{ch_queues=New_dict}.

%%-----------------------------------------------------------------------------
%%
%% @doc fetches particular queue from dict of queues or creates empty one
%%
-spec fetch_job_queue(#ejm{}, any()) -> queue().

fetch_job_queue(#ejm{ch_queues=Data}, Gid) ->
    case dict:find(Gid, Data) of
        {ok, Q} ->
            Q;
        _ ->
            queue:new()
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc stores a children list for the given group to a dictionary
%%
-spec store_spawned_children(#ejm{}, any(), list()) -> #ejm{}.

store_spawned_children(#ejm{ch_data=Data} = St, Gid, Ch) ->
    New_dict = dict:store(Gid, Ch, Data),
    St#ejm{ch_data=New_dict}.

%%-----------------------------------------------------------------------------
%%
%% @doc fetches particular list from dict of lists or creates empty one
%%
-spec fetch_spawned_children(#ejm{}, any()) -> list().

fetch_spawned_children(#ejm{ch_data=Data}, Gid) ->
    case dict:find(Gid, Data) of
        {ok, L} ->
            L;
        _ ->
            []
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc repeatedly calls for creating new child for the given queue
%% until either limit is reached or command queue exhausted.
%% Returns updated queue and spawned children list.
%% @since 2011-07-22 14:54
%%
-spec do_short_command_queue(#ejm{}, {Q, L}, any()) -> {Q, L}.

do_short_command_queue(St, {Q, Ch}, Max) ->
    Len = length(Ch),
    mpln_p_debug:pr({?MODULE, 'do_short_command_queue', ?LINE, Len, Max},
        St#ejm.debug, handler_run, 4),
    case queue:is_empty(Q) of
        false when Len < Max ->
            New_dat = check_one_command(St, {Q, Ch}),
            do_short_command_queue(St, New_dat, Max); % repeat to do commands
        false ->
            mpln_p_debug:pr({?MODULE,
                "do_short_command_queue too many children",
                ?LINE, Len, Max}, St#ejm.debug, handler_run, 2),
            {Q, Ch};
        _ ->
            mpln_p_debug:pr({?MODULE, 'do_short_command_queue no new child',
                ?LINE, Len, Max}, St#ejm.debug, handler_run, 4),
            {Q, Ch}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc returns configured max_children for the given group or default
%%
-spec get_group_max(list(), any(), non_neg_integer()) -> non_neg_integer().

get_group_max(Groups, Gid, Default) ->
    L2 = [X || X <- Groups,
        X#jgroup.id == Gid, is_integer(X#jgroup.max_children)],
    case L2 of
        [I | _] ->
            I#jgroup.max_children;
        _ ->
            Default
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc stores a command into a queue for the given group for later processing.
%% @since 2011-07-22 10:00
%%
-spec store_in_ch_queue(#ejm{}, any(), #job{}) -> #ejm{}.

store_in_ch_queue(St, From, Job) ->
    {Q, Job_g} = fetch_queue(St, Job),
    New_q = queue:in({From, Job_g}, Q),
    store_queue(St, Job_g#job.group, New_q).

%%-----------------------------------------------------------------------------
%%
%% @doc stores the given queue in a dictionary using the given group
%%
-spec store_queue(#ejm{}, default | any(), queue()) -> #ejm{}.

store_queue(#ejm{ch_queues=Data} = St, Gid, Q) ->
    New_data = dict:store(Gid, Q, Data),
    St#ejm{ch_queues=New_data}.

%%-----------------------------------------------------------------------------
%%
%% @doc fetches queue from the state for given job group. Returns queue and
%% job with group data filled
%%
-spec fetch_queue(#ejm{}, #job{}) -> {queue(), #job{}}.

fetch_queue(#ejm{ch_queues=Data} = St, #job{group=Gid} = Job) ->
    Allowed = get_allowed_group(St, Gid),
    case dict:find(Allowed, Data) of
        {ok, Q} ->
            {Q, Job};
        _ ->
            {queue:new(), Job#job{group=Allowed}}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc checks whether the group configured. Returns either the configured
%% value or atom 'default'
%%
get_allowed_group(#ejm{job_groups=L}, Gid) ->
    F = fun(#jgroup{id=Id}) when Id == Gid ->
            true;
        (_) ->
            false
    end,
    case lists:any(F, L) of
        true ->
            Gid;
        false ->
            default
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc checks for a queued command and calls do_one_command for processing
%% if any. Otherwise returns old queue and spawned children.
%% @since 2011-07-22 10:00
%%
-spec check_one_command(#ejm{}, {Q, L}) -> {Q, L}.

check_one_command(St, {Q, Ch}) ->
    mpln_p_debug:pr({?MODULE, 'check_one_command', ?LINE},
        St#ejm.debug, handler_run, 4),
    case queue:out(Q) of
        {{value, Item}, Q2} ->
            New_ch = do_one_command(St, Ch, Item),
            {Q2, New_ch};
        _ ->
            {Q, Ch}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc does command processing in background then sends reply to the client.
%% Returns modified children list with a new child if the one is created.
%% @since 2011-07-15 10:00
%%
-spec do_one_command(#ejm{}, list(), {any(), #job{}}) -> list().

do_one_command(St, Ch, {From, J}) ->
    mpln_p_debug:pr({?MODULE, 'do_one_command cmd', ?LINE, From, J},
        St#ejm.debug, handler_child, 3),
    ejobman_log:log_job(St, J),
    % parameters for ejobman_child
    Child_params = [
        {http_connect_timeout, St#ejm.http_connect_timeout},
        {http_timeout, St#ejm.http_timeout},
        {url_rewrite, St#ejm.url_rewrite},
        {from, From},
        {id, J#job.id},
        {group, J#job.group},
        {method, J#job.method},
        {url, J#job.url},
        {host, J#job.host},
        {params, J#job.params},
        {auth, J#job.auth},
        {debug, St#ejm.debug}
        ],
    Res = supervisor:start_child(ejobman_child_supervisor, [Child_params]),
    mpln_p_debug:pr({?MODULE, 'do_one_command res', ?LINE, Res},
        St#ejm.debug, handler_child, 4),
    case Res of
        {ok, Pid} ->
            add_child(Ch, Pid);
        {ok, Pid, _Info} ->
            add_child(Ch, Pid);
        _ ->
            Ch
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc adds child's pid to the list for later use
%% (e.g.: assign a job, kill, rip, etc...)
%%
-spec add_child(list(), pid()) -> list().

add_child(Children, Pid) ->
    Ch = #chi{pid = Pid, start = now()},
    [Ch | Children].

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
        ch_queues = dict:store("key1", queue:new(), dict:new())
    }.

make_test_data() ->
    {From, Method, Url} = make_test_req(),
    St = make_test_st(From),
    {St, From, Method, Url}
.

do_command_test() ->
    {St, From, Method, Url} = make_test_data(),
    Q_in = queue:in(
        {From, #job{method = Method, url = Url}},
        queue:new()),
    New = do_command(St, From, #job{method = Method, url = Url}),
    Stq = St#ejm{
        ch_queues = dict:store("key2", Q_in, dict:new())
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
