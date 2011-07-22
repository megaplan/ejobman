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

-export([cmd/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").
-include("amqp_client.hrl").

-define(HTTP_TIMEOUT, 15000).

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(Params) ->
    error_logger:info_report({?MODULE, init, Params}),
    timer:sleep(500),
    mpln_p_debug:pr({?MODULE, 'init 1', ?LINE}, [], run, 0),
    C = ejobman_conf:get_config_child(Params),
    mpln_p_debug:pr({?MODULE, 'init 2', ?LINE, C}, [], run, 0),
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE, self()},
        C#child.debug, run, 1),
    {ok, C, ?T}.
%%-----------------------------------------------------------------------------
%%
%% Handling call messages
%% @since 2011-07-21 18:09
%%
-spec handle_call(any(), any(), #ejm{}) -> {stop|reply, any(), any(), any()}.

handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call({cmd, Job}, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'cmd', ?LINE, Job, self()},
        St#child.debug, run, 4),
    New = do_smth(St),
    % 'ok' reply goes to the caller of gen_server api
    % (ejobman_handler_cmd:do_one_long_command). The real requestor is in
    % the From field of the Job tuple.
    {reply, ok, New, ?T};
handle_call(status, _From, St) ->
    {reply, St, St, ?T};
handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'other', ?LINE, _N, self()},
        St#child.debug, run, 4),
    New = do_smth(St),
    {reply, {error, unknown_request}, New, ?T}.
%%-----------------------------------------------------------------------------
%%
%% Handling cast messages
%% @since 2011-07-21 18:09
%%
-spec handle_cast(any(), #ejm{}) -> any().

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(st0p, St) ->
    St;
handle_cast(_, St) ->
    New = do_smth(St),
    {noreply, New, ?T}.
%%-----------------------------------------------------------------------------
terminate(_, State) ->
    ejobman_handler:remove_child(self()),
    mpln_p_debug:pr({?MODULE, terminate, ?LINE, self()},
        State#child.debug, run, 2),
    ok.
%%-----------------------------------------------------------------------------
%%
%% Handling all non call/cast messages
%%
-spec handle_info(any(), #child{}) -> any().

handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, info_timeout, ?LINE, self()},
        State#child.debug, run, 6),
    New = do_smth(State),
    {noreply, New, ?T};
handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, other, ?LINE, _Req, self()},
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
