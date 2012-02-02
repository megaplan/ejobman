%%%
%%% ejobman_group_handler_cmd: payload handling
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
%%% @since 2012-01-11 14:16
%%% @license MIT
%%% @doc functions that handle the payload received via AMQP
%%%

-module(ejobman_group_handler_cmd).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([store_rabbit_cmd/4, process_cmd_result/2, do_waiting_jobs/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("group_handler.hrl").
-include("chi.hrl").
-include("job.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc sends received command to a command handler. Returns updated state.
%% @since 2012-01-11 14:16
%%
-spec store_rabbit_cmd(#egh{}, binary(), binary(), binary()) -> #egh{}.

store_rabbit_cmd(State, Tag, Ref, Bin) ->
    mpln_p_debug:pr({?MODULE, 'store_rabbit_cmd json', ?LINE, Ref, Bin},
        State#egh.debug, msg, 4),
    case catch mochijson2:decode(Bin) of
        {'EXIT', Reason} ->
            mpln_p_debug:pr({?MODULE, 'store_rabbit_cmd error',
                ?LINE, Ref, Reason}, State#egh.debug, run, 2),
            ejobman_rb:send_ack(State#egh.conn, Tag),
            State;
        Data ->
            mpln_p_debug:pr({?MODULE, 'store_rabbit_cmd json dat',
                ?LINE, Ref, Data}, State#egh.debug, json, 3),
            Type = ejobman_data:get_type(Data),
            send_to_estat(Ref, Data),
            proceed_cmd_type(State, Type, Tag, Ref, Data)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc removes terminated jobs from a list of running children
%% @since 2012-01-12 15:15
%%
-spec process_cmd_result(#egh{}, binary() | reference()) -> #egh{}.

process_cmd_result(#egh{ch_queue=Q, ch_run=Ch, group=Gid,
                   conn=Conn, queue=Rqueue} = St, Id) ->
    F = fun(#chi{id=X}) ->
                X == Id
        end,
    {Done, Cont} = lists:partition(F, Ch),
    mpln_p_debug:pr({?MODULE, 'process_cmd_result done', ?LINE, Id, Done},
                    St#egh.debug, handler_job, 3),
    mpln_p_debug:pr({?MODULE, 'process_cmd_result continue', ?LINE, Id, Cont},
                    St#egh.debug, handler_job, 5),

    Len = length(Cont),
    N = ejobman_rb:queue_len(Conn, Rqueue),
    Queued = N + queue:len(Q),
    ejobman_stat:upd_stat_t(Gid, Len, Queued),
    
    St#egh{ch_run=Cont}.

%%-----------------------------------------------------------------------------
%%
%% @doc checks if there are waiting jobs and starts them if yes
%% @since 2012-01-12 15:15
%%
-spec do_waiting_jobs(#egh{}) -> #egh{}.

do_waiting_jobs(St) ->
    do_commands_proceed(St).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc removes auth data from data and sends the rest to ejobman_stat
%%
send_to_estat(Ref, Data) ->
    Info = ejobman_data:get_rest_info(Data),
    Clean = ejobman_data:del_auth_info(Info),
    ejobman_stat:add(Ref, 'message', {'rest_info', Clean}).

%%-----------------------------------------------------------------------------
%%
%% @doc calls ejobman_handler with received command info
%%
-spec proceed_cmd_type(#egh{}, binary(), binary(), binary(), any()) -> #egh{}.

proceed_cmd_type(State, <<"rest">>, Tag, Ref, Data) ->
    Job = make_job(Tag, Ref, Data),
    mpln_p_debug:pr({?MODULE, 'proceed_cmd_type job_id', ?LINE, Job#job.id},
        State#egh.debug, job, 2),
    mpln_p_debug:pr({?MODULE, 'proceed_cmd_type job', ?LINE, Job},
        State#egh.debug, job, 4),
    do_commands(State, Job);

proceed_cmd_type(State, Other, Tag, Ref, _Data) ->
    mpln_p_debug:pr({?MODULE, 'proceed_cmd_type other', ?LINE, Ref, Other},
                    State#egh.debug, run, 2),
    ejobman_rb:send_ack(State#egh.conn, Tag),
    State.

%%-----------------------------------------------------------------------------
%%
%% @doc fills in a #job record
%%
-spec make_job(binary(), binary(), any()) -> #job{}.

make_job(Tag, Ref, Data) ->
    Info = ejobman_data:get_rest_info(Data),
    A = make_job_auth(Info),
    Method = ejobman_data:get_method(Info),
    Url = ejobman_data:get_url(Info),
    Host = ejobman_data:get_host(Info),
    Ip = ejobman_data:get_ip(Info),

    Params = ejobman_data:get_params(Info),
    Flat_params = mpln_misc_web:flatten(Params, true),

    Group = ejobman_data:get_group(Info),
    New = A#job{
        id = Ref,
        tag = Tag,
        method = Method,
        url = Url,
        host = Host,
        ip = Ip,
        params = Flat_params,
        group = Group
    },
    fill_id(New).

%%-----------------------------------------------------------------------------
%%
%% @doc creates a #job record with auth data filled in
%%
-spec make_job_auth(any()) -> #job{}.

make_job_auth(Info) ->
    Auth = ejobman_data:get_auth_info(Info),
    Type = ejobman_data:get_auth_type(Auth),
    Str = mpln_misc_web:make_string(Type),
    #job{
        auth = fill_auth_data(Str, Auth)
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a filled #auth record
%%
-spec fill_auth_data(any(), any()) -> #auth{}.

fill_auth_data("megaplan", Auth) ->
    F = fun ({<<"type">>, _}) -> false;
            ({"type", _})     -> false;
            ({_, _})          -> true;
            (_)               -> false
    end,
    List = ejobman_data:get_auth_data_list(Auth),
    Data = lists:filter(F, List),
    {A, S} = ejobman_data:get_auth_keys(Auth),
    #auth{type='megaplan', data = Data, auth_key = A, secret_key = S};

fill_auth_data(_, Auth) ->
    User = ejobman_data:get_auth_user(Auth),
    Pass = ejobman_data:get_auth_password(Auth),
    #auth{type='basic', user = User, password = Pass}.

%%-----------------------------------------------------------------------------
%%
%% @doc stores a job in the queue, processes locally queued jobs.
%% There should be 0 or 1 job queued locally because amqp prefetch is set to 1.
%%
-spec do_commands(#egh{}, #job{}) -> #egh{}.

do_commands(#egh{ch_queue=Q} = State, Job) ->
    Q2 = queue:in(Job, Q),
    do_commands_proceed(State#egh{ch_queue=Q2}).

%%-----------------------------------------------------------------------------
%%
%% @doc repeatedly calls for creating a new child
%% until either the limit is reached or the local queue exhausted.
%% Returns an updated state
%%
-spec do_commands_proceed(#egh{}) -> #egh{}.

do_commands_proceed(#egh{ch_queue=Q, max=Max, ch_run=Ch, id=Id, group=Gid,
                        conn=Conn, queue=Rqueue} = St) ->
    Len = length(Ch),
    mpln_p_debug:pr({?MODULE, 'do_command_proceed', ?LINE, Id, Gid, Len, Max},
                    St#egh.debug, run, 4),
    case queue:is_empty(Q) of
        false when Len < Max ->
            New = do_one_command(St, Len),
            do_commands_proceed(New);
        false ->
            Qlen = queue:len(Q),
            N = ejobman_rb:queue_len(Conn, Rqueue),
            mpln_p_debug:pr({?MODULE, 'do_command_proceed too many children',
                             ?LINE, Id, Gid, N, Qlen, Len, Max},
                            St#egh.debug, run, 2),
            St;
        _ ->
            mpln_p_debug:pr({?MODULE, 'do_command_proceed empty internal queue',
                             ?LINE, Id, Gid, Len, Max},
                            St#egh.debug, run, 4),
            St
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc creates one child for queued job
%%
-spec do_one_command(#egh{}, non_neg_integer()) -> #egh{}.

do_one_command(#egh{ch_queue=Q, ch_run=Ch, max=Max, group=Gid,
                   conn=Conn, queue=Rqueue} = St, Len) ->
    {{value, Job}, Q2} = queue:out(Q),
    N = ejobman_rb:queue_len(Conn, Rqueue),
    Queued = N + queue:len(Q),
    ejobman_stat:add(Job#job.id, 'from_queue',
                             [{max, Max},
                              {running, Len},
                              {queued, Queued},
                              {group, Gid}]),
    New_ch = do_one_command_real(St, Ch, Job),
    Len2 = length(New_ch),
    ejobman_stat:upd_stat_t(Gid, Len2, Queued - (Len2 - Len)),
    St#egh{ch_queue=Q2, ch_run=New_ch}.

%%-----------------------------------------------------------------------------
%%
%% @doc does command processing in background.
%% Returns modified children list with a new child if the one is created.
%% @since 2012-01-11 16:40
%%
-spec do_one_command_real(#egh{}, [C], #job{}) -> [C].

do_one_command_real(St, Ch, J) ->
    mpln_p_debug:pr({?MODULE, 'do_one_command_real job_id', ?LINE, J#job.id},
        St#egh.debug, handler_child, 2),
    mpln_p_debug:pr({?MODULE, 'do_one_command_real', ?LINE, J},
        St#egh.debug, handler_child, 3),
    % parameters for ejobman_child
    Child_params = [
        {gh_pid, self()},
        {http_connect_timeout, St#egh.http_connect_timeout},
        {http_timeout, St#egh.http_timeout},
        {schema_rewrite, St#egh.schema_rewrite},
        {url_rewrite, St#egh.url_rewrite},
        {id, J#job.id},
        {tag, J#job.tag},
        {group, J#job.group},
        {method, J#job.method},
        {url, J#job.url},
        {host, J#job.host},
        {ip, J#job.ip},
        {params, J#job.params},
        {auth, J#job.auth},
        {debug, St#egh.debug}
        ],
    mpln_p_debug:pr({?MODULE, 'do_one_command_real child params', ?LINE,
        Child_params}, St#egh.debug, handler_child, 4),
    Res = supervisor:start_child(ejobman_child_supervisor, [Child_params]),
    mpln_p_debug:pr({?MODULE, 'do_one_command_real res', ?LINE, Res},
        St#egh.debug, handler_child, 5),
    case Res of
        {ok, Pid} ->
            add_child(Ch, Pid, J#job.id, J#job.tag);
        {ok, Pid, _Info} ->
            add_child(Ch, Pid, J#job.id, J#job.tag);
        _ ->
            mpln_p_debug:pr({?MODULE, 'do_one_command_real res', ?LINE, 'error',
                J#job.id, J#job.group, Res}, St#egh.debug, handler_child, 1),
            Ch
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc adds child's pid to the list for later use
%% (e.g.: assign a job, send ack to rabbit, kill, rip, etc...)
%%
-spec add_child([#chi{}], pid(), reference(), binary()) -> [#chi{}].

add_child(Children, Pid, Id, Tag) ->
    Ch = #chi{pid = Pid, id = Id, start = now(), tag = Tag},
    [Ch | Children].

%%-----------------------------------------------------------------------------
%%
%% @doc fills job id if it is undefined
%%
fill_id(#job{id=undefined} = Job) ->
    Job#job{id=make_ref()};

fill_id(Job) ->
    Job.

%%-----------------------------------------------------------------------------
