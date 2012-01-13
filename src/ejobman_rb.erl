%%%
%%% ejobman_rb: RabbitMQ interaction
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
%%% @doc RabbitMQ interaction
%%%

-module(ejobman_rb).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include_lib("amqp_client.hrl").
-include("rabbit_session.hrl").

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/1]).
-export([teardown/1, teardown_channel/1, send_reply/4]).
-export([send_ack/2]).
-export([start_channel/2, create_queue/2, create_exchange/3, bind_queue/4]).
-export([setup_consumer/2, cancel_consumer/2]).
-export([start_receiver/1, make_prop_id/1, get_prop_id/1]).
-export([send_message/4, send_message/5, send_message2/4]).
-export([channel_qos/3, send_dur_message/4, send_dur_message/5]).
-export([queue_len/2]).

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

-define(SETUP_CONSUMER_TIMEOUT, 10000).

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc gets queue message count by declaring existing queue
%% @since 2012-01-13 20:11
%%
-spec queue_len(#conn{}, binary()) -> non_neg_integer().

queue_len(Conn, Queue) ->
    #'queue.declare_ok'{queue=Queue, message_count=N} =
        create_queue(Conn, Queue),
    N.

%%-----------------------------------------------------------------------------
%%
%% @doc starts full amqp receiver:
%% connection, channel, fanout exchange, queue, binding
%% @since 2011-01-10 17:15
%%
-spec start_receiver(#rses{}) -> {ok, #conn{}}.

start_receiver(Rses) ->
    start(Rses#rses{exchange_type= <<"fanout">>}).

%%-----------------------------------------------------------------------------
%%
%% @doc starts new channel in the existing AMQP connection
%% @since 2011-12-31 16:42
%%
-spec start_channel(#conn{}, binary()) -> {ok, #conn{}}.

start_channel(#conn{connection=Connection} = Conn, Vhost) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Access = #'access.request'{realm = Vhost,
        exclusive = false,
        passive = true,
        active = true,
        write = true,
        read = true},
    #'access.request_ok'{ticket = Ticket} = amqp_channel:call(Channel, Access),
    {ok, Conn#conn{channel=Channel, ticket=Ticket}}.

%%-----------------------------------------------------------------------------
%%
%% @doc declares an amqp queue
%% @since 2011-12-30 17:03
%%
-spec create_queue(#conn{}, binary()) -> #'queue.declare_ok'{}.

create_queue(#conn{channel=Channel, ticket=Ticket}, Q) ->
    QueueDeclare = #'queue.declare'{ticket = Ticket, queue = Q,
        passive = false, durable = true,
        exclusive = false, auto_delete = false,
        nowait = false, arguments = []},
    Res = amqp_channel:call(Channel, QueueDeclare),
    #'queue.declare_ok'{queue = Q} = Res,
    Res.

%%-----------------------------------------------------------------------------
%%
%% @doc declares an amqp exchange
%% @since 2011-12-30 17:03
%%
-spec create_exchange(#conn{}, binary(), binary()) -> any().

create_exchange(#conn{channel=Channel, ticket=Ticket}, X, Xtype) ->
    ExchangeDeclare = #'exchange.declare'{ticket = Ticket,
        exchange = X, type= Xtype,
        passive = false, durable = true,
        auto_delete=false, internal = false,
        nowait = false, arguments = []},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare).

%%-----------------------------------------------------------------------------
%%
%% @doc binds queue to exchange
%% @since 2011-12-30 17:13
%%
-spec bind_queue(#conn{}, binary(), binary(), false | binary()) -> any().

bind_queue(#conn{channel=Channel, ticket=Ticket}, Q, X, Key) ->
    QueueBind = #'queue.bind'{ticket = Ticket,
        queue = Q,
        exchange = X,
        routing_key = Key,
        nowait = false, arguments = []},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind).

%%-----------------------------------------------------------------------------
%%
%% @doc does all the AMQP client preparations, namely: connection, channel,
%% queue, exchange, binding.
%% @since 2011-07-15
%%
-spec start(#rses{}) -> {ok, #conn{}}.

start(Rses) ->
    Host = Rses#rses.host,
    Port = Rses#rses.port,
    User = Rses#rses.user,
    Password = Rses#rses.password,
    Vhost = Rses#rses.vhost,
    {ok, Connection} = amqp_connection:start(network, #amqp_params{
        username = User,
        password = Password,
        host = Host,
        port = Port,
        virtual_host = Vhost
        }),

    {ok, Conn} = start_channel(#conn{connection=Connection}, Vhost),

    Q = Rses#rses.queue,
    X = Rses#rses.exchange,
    Xtype = Rses#rses.exchange_type,
    BindKey = Rses#rses.routing_key,

    create_queue(Conn, Q),
    create_exchange(Conn, X, Xtype),
    bind_queue(Conn, Q, X, BindKey),
    New_conn = setup_consumer(Conn, Q),
    {ok, New_conn}.

%%-----------------------------------------------------------------------------
%%
%% @doc stops amqp channel
%% @since 2011-12-30 18:09
%%
-spec teardown_channel(#conn{}) -> ok.

teardown_channel(#conn{channel = Channel, consumer_tag = ConsumerTag}) ->
    cancel_consumer(Channel, ConsumerTag),
    amqp_channel:close(Channel).

%%-----------------------------------------------------------------------------
%%
%% @doc cancels consumer, closes channel, closes connection
%% @since 2011-07-15
%%
teardown(#conn{connection = Connection,
        channel = Channel,
        consumer_tag = ConsumerTag}) ->
    cancel_consumer(Channel, ConsumerTag),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection)
.
%%-----------------------------------------------------------------------------
%%
%% @doc sends reply with particular routing key
%% @since 2011-07-15
%%
send_reply(Channel, X, Rt_key, Payload) ->
    io:format("send_reply rt, payload:~n~p~n~p~n", [Rt_key, Payload]),
    send_message(Channel, X, Rt_key, Payload)
.
%%-----------------------------------------------------------------------------
%%
%% @doc sends acknowledge for AMQP message.
%% @since 2011-07-15
%%
-spec send_ack(#conn{}, any()) -> any().

send_ack(Conn, Tag) ->
    Channel = Conn#conn.channel,
    ok = amqp_channel:call(Channel, #'basic.ack'{delivery_tag = Tag}).

%%-----------------------------------------------------------------------------
%%
%% @doc setups consumer for given queue at given exchange
%% @since 2011-07-15
%%
-spec setup_consumer(#conn{}, binary()) -> #conn{}.

setup_consumer(#conn{channel=Channel} = Conn, Q) ->
    BasicConsume = #'basic.consume'{queue = Q, no_ack = false },
    #'basic.consume_ok'{consumer_tag = ConsumerTag}
        = amqp_channel:subscribe(Channel, BasicConsume, self()),
    Conn#conn{consumer_tag=ConsumerTag}.

%%-----------------------------------------------------------------------------
%%
%% @doc cancels consumer
%% @since 2011-07-15
%%
-spec cancel_consumer(any(), any()) -> any().

cancel_consumer(Channel, ConsumerTag) ->
    BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag, nowait = false},
    #'basic.cancel_ok'{consumer_tag = ConsumerTag} =
        amqp_channel:call(Channel,BasicCancel)
.
%%-----------------------------------------------------------------------------
%%
%% @doc creates amqp basic property with given id
%%
-spec make_prop_id(binary()) -> #'P_basic'{}.

make_prop_id(Id) ->
    #'P_basic'{message_id = Id}.

%%-----------------------------------------------------------------------------
%%
%% @doc extracts id from amqp basic property
%%
-spec get_prop_id(#'P_basic'{}) -> binary().

get_prop_id(Props) ->
    Props#'P_basic'.message_id.

%%-----------------------------------------------------------------------------
%%
%% @doc publishes persistent AMQP message with given payload to exchange
%% @since 2011-07-15
%%
-spec send_dur_message(binary(), binary(), binary(), binary()) -> ok.

send_dur_message(Channel, X, RoutingKey, Payload, Id) ->
    Pr = make_prop_id(Id),
    Props = Pr#'P_basic'{delivery_mode = 2},
    send_message(Channel, X, RoutingKey, Payload, Props).

send_dur_message(Channel, X, RoutingKey, Payload) ->
    Props = #'P_basic'{delivery_mode = 2},
    send_message(Channel, X, RoutingKey, Payload, Props).

%%
%% @doc publishes AMQP message with given payload (and properties) to exchange
%% @since 2011-07-15
%%
-spec send_message(binary(), binary(), binary(), binary()) -> ok.

send_message(Channel, X, RoutingKey, Payload, #'P_basic'{} = Props) ->
    Msg = #amqp_msg{payload = Payload, props = Props},
    send_message2(Channel, X, RoutingKey, Msg);

send_message(Channel, X, RoutingKey, Payload, Id) ->
    Props = make_prop_id(Id),
    Msg = #amqp_msg{payload = Payload, props = Props},
    send_message2(Channel, X, RoutingKey, Msg).

send_message(Channel, X, RoutingKey, Payload) ->
    Msg = #amqp_msg{payload = Payload},
    send_message2(Channel, X, RoutingKey, Msg).

%%
%% @doc publishes given AMQP message to exchange
%%
-spec send_message2(binary(), binary(), binary(), #amqp_msg{}) -> ok.

send_message2(Channel, X, RoutingKey, Msg) ->
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    amqp_channel:cast(Channel, Publish, Msg).

%%-----------------------------------------------------------------------------
%%
%% @doc sets qos parameters for the channel
%% @since 2012-01-11 13:52
%%
channel_qos(#conn{channel=Channel}, Size, Cnt) ->
    Q = #'basic.qos'{prefetch_size=Size, prefetch_count=Cnt, global=false},
    #'basic.qos_ok'{} = amqp_channel:call(Channel, Q).

%%-----------------------------------------------------------------------------
