%%%
%%% ejobman_receiver: AMQP messages receiver
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
%%% @doc Receive messages from a RabbitMQ queue and call ejobman_handler
%%% for the rest of a job
%%% 

-module(ejobman_receiver).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).
-export([logrotate/0]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("ejobman.hrl").
-include("amqp_client.hrl").

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(_) ->
    C = ejobman_conf:get_config(),
    New = prepare_all(C),
    process_flag(trap_exit, true), % to perform amqp teardown
    mpln_p_debug:pr({'init done', ?MODULE, ?LINE}, New#ejm.debug, run, 1),
    {ok, New, ?T}.

%------------------------------------------------------------------------------
-spec handle_call(any(), any(), #ejm{}) -> {stop|reply, any(), any(), any()}.
%%
%% Handling call messages
%% @since 2011-07-15 11:00
%%
handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call(status, _From, St) ->
    {reply, St, St, ?T};
handle_call(_N, _From, St) ->
    mpln_p_debug:p("~p::~p other:~n~p~n",
        [?MODULE, ?LINE, _N], St#ejm.debug, run, 4),
    {reply, {error, unknown_request}, St, ?T}.

%------------------------------------------------------------------------------
-spec handle_cast(any(), #ejm{}) -> any().
%%
%% Handling cast messages
%% @since 2011-07-15 11:00
%%
handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(logrotate, St) ->
    mpln_misc_log:prepare_log(St#ejm.log),
    {noreply, St, ?T};
handle_cast(st0p, St) ->
    St;
handle_cast({test, Payload}, State) ->
    New = ejobman_receiver_cmd:store_rabbit_cmd(State, Payload),
    {noreply, New, ?T};
handle_cast(_Other, St) ->
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _Other},
        St#ejm.debug, run, 2),
    {noreply, St, ?T}.

%------------------------------------------------------------------------------
terminate(_, #ejm{conn=Conn} = State) ->
    ejobman_rb:teardown(Conn),
    mpln_p_debug:pr({?MODULE, terminate, ?LINE}, State#ejm.debug, run, 1),
    ok.

%------------------------------------------------------------------------------
-spec handle_info(any(), #ejm{}) -> any().
%%
%% Handling all non call/cast messages
%%
handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, 'info_timeout', ?LINE}, State#ejm.debug, run, 6),
    {noreply, State, ?T};
handle_info({#'basic.deliver'{delivery_tag = _Tag}, Content} = _Req, State) ->
    mpln_p_debug:p("~p::~p basic.deliver:~n~p~n",
        [?MODULE, ?LINE, _Req], State#ejm.debug, run, 5),
    Payload = Content#amqp_msg.payload,
    ejobman_rb:send_ack(State#ejm.conn, _Tag),
    New = ejobman_receiver_cmd:store_rabbit_cmd(State, Payload),
    {noreply, New, ?T};
handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, 'other', ?LINE, _Req}, State#ejm.debug, run, 3),
    {noreply, State, ?T}.

%------------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
-spec start() -> any().
%%
%% @doc starts receiver gen_server
%% @since 2011-07-15 10:00
%%
start() ->
    start_link().

%%-----------------------------------------------------------------------------
%%
%% @doc starts receiver gen_server with pre-defined config
%% @since 2011-07-15 10:00
%%
-spec start_link() -> any().

start_link() ->
    start_link(?CONF).

%%
%% @doc starts receiver gen_server with given config
%% @since 2011-07-15 10:00
%%
-spec start_link(string()) -> any().

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%-----------------------------------------------------------------------------
%%
%% @doc stops receiver gen_server
%% @since 2011-07-15 10:00
%%
-spec stop() -> any().

stop() ->
    gen_server:call(?MODULE, stop).

%%-----------------------------------------------------------------------------
%%
%% @doc sends message to receiver to rotate logs
%% @since 2011-08-10 16:00
%%
-spec logrotate() -> ok.

logrotate() ->
    gen_server:cast(?MODULE, logrotate).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc does all necessary preparations: [re]opens log file.
%% @since 2011-07-15
%%
-spec prepare_all(#ejm{}) -> #ejm{}.

prepare_all(C) ->
    prepare_log(C),
    prepare_q(C).

%%-----------------------------------------------------------------------------
%%
%% @doc Prepare RabbitMQ exchange, queue, consumer
%% @since 2011-07-15
%%
-spec prepare_q(#ejm{}) -> #ejm{}.

prepare_q(C) ->
    {ok, Conn} = ejobman_rb:start(C#ejm.rses),
    C#ejm{conn=Conn}.

%%-----------------------------------------------------------------------------
%%
%% @doc prepare log if it is defined
%% @since 2011-09-01 17:14
%%
-spec prepare_log(#ejm{}) -> ok.

prepare_log(#ejm{log=undefined}) ->
    ok;
prepare_log(#ejm{log=Log}) ->
    mpln_misc_log:prepare_log(Log).

%%-----------------------------------------------------------------------------
