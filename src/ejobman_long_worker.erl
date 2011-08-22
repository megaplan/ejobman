%%%
%%% ejobman_long_worker: dynamically added worker
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
%%% @since 2011-07-21 18:09
%%% @license MIT
%%% @doc dynamically added worker that does the real thing.
%%%

-module(ejobman_long_worker).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([cmd/2, cmd/3, get_os_pid/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").
%-include("amqp_client.hrl").

-define(HTTP_TIMEOUT, 15000).

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(Params) ->
    C = ejobman_conf:get_config_child(Params),
    New = add_port(C),
    % to catch exit on port close by external process. Is it really necessary?
    process_flag(trap_exit, true),
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE, self(), C#child.id},
        C#child.debug, run, 2),
    {ok, New, ?T}.

%%-----------------------------------------------------------------------------
%%
%% Handling call messages
%% @since 2011-07-21 18:09
%%
-spec handle_call(any(), any(), #child{}) -> {stop|reply, any(), any(), any()}.

handle_call(get_os_pid, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'get_os_pid', ?LINE, St#child.id},
        St#child.debug, run, 4),
    New = do_smth(St),
    {reply, New#child.os_pid, New, ?T};
handle_call({cmd2, Job}, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'cmd2', ?LINE, Job, St#child.id},
        St#child.debug, run, 4),
    Res_cmd = process_cmd2(St, Job),
    mpln_p_debug:pr({?MODULE, 'cmd2 res', ?LINE, Res_cmd},
        St#child.debug, run, 4),
    New = do_smth(St),
    {reply, ok, New, ?T};
handle_call(stop, _From, St) ->
    catch port_close(St#child.port),
    {stop, normal, ok, St};
handle_call({cmd, Job}, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'cmd', ?LINE, Job, St#child.id},
        St#child.debug, run, 4),
    process_cmd(St, Job),
    New = do_smth(St),
    % 'ok' reply goes to the caller of gen_server api
    % (ejobman_handler_cmd:do_one_long_command). The real requestor is in
    % the From field of the Job tuple.
    {reply, ok, New, ?T};
handle_call(status, _From, St) ->
    {reply, St, St, ?T};
handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'call other', ?LINE, _N, St#child.id},
        St#child.debug, run, 4),
    New = do_smth(St),
    {reply, {error, unknown_request}, New, ?T}.

%%-----------------------------------------------------------------------------
%%
%% Handling cast messages
%% @since 2011-07-21 18:09
%%
-spec handle_cast(any(), #child{}) -> any().

handle_cast(stop, St) ->
    catch port_close(St#child.port),
    {stop, normal, St};
handle_cast(st0p, St) ->
    St;
handle_cast(_Req, St) ->
    New = do_smth(St),
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _Req, St#child.id},
        St#child.debug, run, 4),
    {noreply, New, ?T}.

%%-----------------------------------------------------------------------------
terminate(_R, St) ->
    catch port_close(St#child.port),
    mpln_p_debug:pr({?MODULE, 'terminate done', ?LINE, _R, St#child.id},
        St#child.debug, run, 4),
    ok.

%%-----------------------------------------------------------------------------
%%
%% Handling all non call/cast messages
%%
-spec handle_info(any(), #child{}) -> any().

handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, info_timeout, ?LINE, State#child.id},
        State#child.debug, run, 6),
    New = do_smth(State),
    {noreply, New, ?T};
handle_info({P1, {data, Data}}, #child{port=P2, os_pid=undefined} = St)
        when P1 =:= P2->
    mpln_p_debug:pr({?MODULE, 'info own port data', ?LINE, P1, Data,
        St#child.id}, St#child.debug, run, 2),
    Std = process_port_data(St, Data),
    New = do_smth(Std),
    {noreply, New, ?T};
handle_info({P1, {exit_status, Code}}, #child{port=P2} = St) when P1 =:= P2->
    mpln_p_debug:pr({?MODULE, 'info own port exit', ?LINE, P1, Code,
        St#child.id}, St#child.debug, run, 2),
    New = do_smth(St#child{port=undefined}),
    {stop, normal, New};
handle_info({Port, {exit_status, Code}}, State) ->
    mpln_p_debug:pr({?MODULE, 'info other port exit', ?LINE, Port, Code,
        State#child.id}, State#child.debug, run, 2),
    New = do_smth(State),
    {noreply, New, ?T};
handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, 'info other', ?LINE, _Req, State#child.id},
        State#child.debug, run, 3),
    New = do_smth(State),
    {noreply, New, ?T}.

%%-----------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
start() ->
    start_link().
%%-----------------------------------------------------------------------------
start_link() ->
    start_link([]).
start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).
%%-----------------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).

%%-----------------------------------------------------------------------------
%%
%% @doc transmit the command to a gen_server with the given pid
%% @since 2011-07-22 19:00
%%
-spec cmd(pid(), tuple()) -> ok.

cmd(Pid, Job) ->
    gen_server:call(Pid, {cmd, Job}).

%%
%% @doc transmit the command to a gen_server with the given pid. Uses timeout
%% @since 2011-08-03 14:03
%%
-spec cmd(pid(), tuple(), non_neg_integer()) -> ok.

cmd(Pid, Job, Timeout) ->
    gen_server:call(Pid, {cmd, Job}, Timeout).

%%-----------------------------------------------------------------------------
%%
%% @doc gets os pid from the workers
%% @since 2011-08-19 14:09
%%
get_os_pid(Pid) ->
    gen_server:call(Pid, get_os_pid).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc processes command, then sends stop message to itself
%% @since 2011-07-21 18:09
%%
-spec do_smth(#child{}) -> #child{}.

do_smth(State) ->
    State.

%%-----------------------------------------------------------------------------
%%
%% @doc processes received command. Returns result to the real requestor
%% @todo make it async
%%
process_cmd(St, {From, Method, Url}) ->
    real_cmd(St#child{method=Method, url=Url, from=From})
.
%%-----------------------------------------------------------------------------
real_cmd(#child{method = Method_bin, url = Url, from = From} = St) ->
    mpln_p_debug:pr({?MODULE, 'process_cmd params', ?LINE, St#child.id,
        Method_bin, Url, From}, St#child.debug, run, 3),
    Method = ejobman_clean:get_method(Method_bin),
    Res = http:request(Method, {Url, []},
        [{timeout, ?HTTP_TIMEOUT}, {connect_timeout, ?HTTP_TIMEOUT}],
        []),
    gen_server:reply(From, Res),
    mpln_p_debug:pr({?MODULE, 'process_cmd res', ?LINE, St#child.id, Res},
        St#child.debug, run, 4)
.
%%-----------------------------------------------------------------------------
process_cmd2(St, stop) ->
    mpln_p_debug:pr({?MODULE, 'process_cmd2 close', ?LINE},
        St#child.debug, run, 0),
    gen_server:cast(self(), stop),
    catch port_close(St#child.port)
;
process_cmd2(#child{port=P} = St, Data) ->
    mpln_p_debug:pr({?MODULE, 'process_cmd2 other', ?LINE},
        St#child.debug, run, 0),
    port_command(P, Data)
.
%%-----------------------------------------------------------------------------
add_port(C) ->
    mpln_p_debug:pr({?MODULE, 'add_port',
        ?LINE, C#child.name, C#child.id}, C#child.debug, run, 2),
    Id = cr_port(C#child.name),
    C#child{port=Id}
.
%%-----------------------------------------------------------------------------
cr_port(File) ->
    Name = {spawn, File},
    Settings = [
        exit_status,
        use_stdio,
        hide,
        %{packet, 2}
        {line, 80}
    ],
    open_port(Name, Settings).

%%-----------------------------------------------------------------------------
process_port_data(#child{} = St, {noeol, _Line}) ->
    St;
process_port_data(St, {eol, [$c, $u, $r, $_, $p, $i, $d, $= | _] = Line}) ->
    % Line: "cur_pid=15682"
    case string:tokens(Line, "=") of
        [_, Str | _] ->
            N = list_to_integer(Str),
            St#child{os_pid=N};
        _ ->
            St
    end;
process_port_data(St, _) ->
    St.

%%-----------------------------------------------------------------------------
