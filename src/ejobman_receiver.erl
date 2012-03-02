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
-export([send_ack/2]).
-export([tell_group/3, get_conn_params/0]).
-export([
         reload_config_signal/0
        ]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("receiver.hrl").
-include("amqp_client.hrl").

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(_) ->
    C = ejobman_conf:get_config_receiver(),
    New = prepare_all(C),
    process_flag(trap_exit, true), % to perform amqp teardown
    mpln_p_debug:pr({'init done', ?MODULE, ?LINE}, New#ejr.debug, run, 1),
    {ok, New}.

%------------------------------------------------------------------------------
-spec handle_call(any(), any(), #ejr{}) ->
                         {stop, normal, ok, #ejr{}}
                             | {reply, any(), #ejr{}}.
%%
%% Handling call messages
%% @since 2011-07-15 11:00
%%
handle_call(stop, _From, St) ->
    {stop, normal, ok, St};

handle_call(status, _From, St) ->
    {reply, St, St};

handle_call(get_conn_params, _From, St) ->
    Res = ejobman_receiver_cmd:get_conn_params(St),
    {reply, Res, St};

handle_call({set_debug_item, Facility, Level}, _From, St) ->
    % no api for this, use message passing
    New = mpln_misc_run:update_debug_level(St#ejr.debug, Facility, Level),
    {reply, St#ejr.debug, St#ejr{debug=New}};

handle_call(_N, _From, St) ->
    mpln_p_debug:p("~p::~p other:~n~p~n",
        [?MODULE, ?LINE, _N], St#ejr.debug, run, 2),
    {reply, {error, unknown_request}, St}.

%------------------------------------------------------------------------------
-spec handle_cast(any(), #ejr{}) -> any().
%%
%% Handling cast messages
%% @since 2011-07-15 11:00
%%
handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast(logrotate, St) ->
    prepare_log(St),
    {noreply, St};

handle_cast({tell_group, Gid, Exchange, Key}, St) ->
    mpln_p_debug:pr({?MODULE, 'tell_group', ?LINE, Gid, Exchange, Key},
        St#ejr.debug, run, 3),
    New = store_group(St, Gid, Exchange, Key),
    {noreply, New};

handle_cast({send_ack, Id, Tag}, #ejr{conn=Conn} = St) ->
    Res = ejobman_rb:send_ack(Conn, Tag),
    mpln_p_debug:pr({?MODULE, 'send_ack res', ?LINE, Id, Tag, Res},
        St#ejr.debug, msg, 2),
    {noreply, St};

handle_cast(reload_config_signal, St) ->
    New = process_reload_config(St),
    {noreply, New};

handle_cast(_Other, St) ->
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _Other},
        St#ejr.debug, run, 2),
    {noreply, St}.

%------------------------------------------------------------------------------
terminate(_, #ejr{conn=Conn, pid_file=File} = State) ->
    ejobman_rb:teardown(Conn),
    mpln_misc_run:remove_pid(File),
    mpln_p_debug:pr({?MODULE, terminate, ?LINE}, State#ejr.debug, run, 1),
    ok.

%------------------------------------------------------------------------------
-spec handle_info(any(), #ejr{}) -> any().
%%
%% Handling all non call/cast messages
%%
handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, 'info_timeout', ?LINE}, State#ejr.debug, run, 6),
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag=Tag, routing_key=Gid}, Content} =
            _Req, #ejr{conn=Conn} = State) ->
    Ref = make_ref(),
    mpln_p_debug:pr({?MODULE, 'basic.deliver', ?LINE, Ref, _Req},
                    State#ejr.debug, msg, 3),
    Payload = Content#amqp_msg.payload,
    Props = Content#amqp_msg.props,
    erpher_et:trace_me(50, ?MODULE, Gid, forward, {Ref, Content}),
    ejobman_stat:add(Ref, 'start', {'start', Props#'P_basic'.timestamp}),
    ejobman_receiver_cmd:push_message(State, Gid, Ref, Payload),
    ejobman_rb:send_ack(Conn, Tag),
    {noreply, State};

handle_info(#'basic.consume_ok'{consumer_tag = Tag}, State) ->
    New = ejobman_receiver_cmd:store_consumer_tag(State, Tag),
    {noreply, New};

handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, 'other', ?LINE, _Req}, State#ejr.debug, run, 2),
    {noreply, State}.

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

%%-----------------------------------------------------------------------------
%%
%% @doc sends a tag to receiver to send acknowledge to amqp. Id here is
%% for message trace only.
%% @since 2011-12-02 16:16
%%
-spec send_ack(reference(), any()) -> ok.

send_ack(Id, Tag) ->
    gen_server:cast(?MODULE, {send_ack, Id, Tag}).

%%-----------------------------------------------------------------------------
%%
%% @doc forwards group info (id, exchange, routing key) to the server
%% @since 2012-01-10 15:12
%%
-spec tell_group(default | binary(), binary(), binary()) -> ok.

tell_group(Gid, Exchange, Key) ->
    gen_server:cast(?MODULE, {tell_group, Gid, Exchange, Key}).

%%-----------------------------------------------------------------------------
%%
%% @doc returns vhost and connection parameters
%% @since 2012-01-10 14:55
%%
-spec get_conn_params() -> {binary(), any()}.

get_conn_params() ->
    gen_server:call(?MODULE, get_conn_params).

%%-----------------------------------------------------------------------------
%%
%% @doc send a message to the server to reload own config
%% @since 2012-03-02 13:53
%%
-spec reload_config_signal() -> ok.

reload_config_signal() ->
    gen_server:cast(?MODULE, reload_config_signal).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc does all necessary preparations: [re]opens log file.
%% @since 2011-07-15
%%
-spec prepare_all(#ejr{}) -> #ejr{}.

prepare_all(C) ->
    New = prepare_part(C),
    prepare_q(New).

-spec prepare_part(#ejr{}) -> #ejr{}.

prepare_part(C) ->
    New = C#ejr{pid=self()},
    prepare_log(New),
    write_pid(New),
    log_sys_info(New),
    New.

%%-----------------------------------------------------------------------------
%%
%% @doc logs erlang vm pid and local host name
%%
log_sys_info(C) ->
    mpln_p_debug:pr({?MODULE, 'prepare_all pid', ?LINE, os:getpid()},
                    C#ejr.debug, run, 0),
    mpln_p_debug:pr({?MODULE, 'prepare_all localhost', ?LINE,
                     net_adm:localhost()}, C#ejr.debug, run, 0).

%%-----------------------------------------------------------------------------
%%
%% @doc Prepare RabbitMQ exchange, queue, consumer
%% @since 2011-07-15
%%
-spec prepare_q(#ejr{}) -> #ejr{}.

prepare_q(C) ->
    {ok, Conn} = ejobman_rb:start_receiver(C#ejr.rses),
    C#ejr{conn=Conn}.

%%-----------------------------------------------------------------------------
%%
%% @doc prepare log if it is defined
%% @since 2011-09-01 17:14
%%
-spec prepare_log(#ejr{}) -> ok.

prepare_log(#ejr{log=undefined}) ->
    ok;
prepare_log(#ejr{log=Log}) ->
    mpln_misc_log:prepare_log(Log).

%%-----------------------------------------------------------------------------
%%
%% @doc writes pid file
%% @since 2011-11-11 14:17
%%
-spec write_pid(#ejr{}) -> ok.

write_pid(#ejr{pid_file=undefined}) ->
    ok;
write_pid(#ejr{pid_file=File}) ->
    mpln_misc_run:write_pid(File).

%%-----------------------------------------------------------------------------
%%
%% @doc stores group info in the state
%%
-spec store_group(#ejr{}, any(), binary(), binary()) -> #ejr{}.

store_group(#ejr{groups=Groups} = St, Gid, Exchange, Key) ->
    New = lists:keystore(Gid, 1, Groups, {Gid, Exchange, Key}),
    St#ejr{groups=New}.

%%-----------------------------------------------------------------------------
%%
%% @doc fetches config from updated environment and stores it in the state.
%% We don't reconnect to amqp here as this requires group handlers restart
%% which we avoid by all means
%%
-spec process_reload_config(#ejr{}) -> #ejr{}.

process_reload_config(#ejr{rses=Rses, conn=Conn, pid_file=File}) ->
    mpln_misc_run:remove_pid(File),

    C = ejobman_conf:get_config_receiver(),
    prepare_part(C#ejr{rses=Rses, conn=Conn}).

%%-----------------------------------------------------------------------------
