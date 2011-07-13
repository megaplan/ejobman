%%% RabbitMQ interaction
-module(ejobman_rb).

-include_lib("amqp_client.hrl").
-include("rabbit_session.hrl").

-export([start/1]).
-export([teardown/1, send_reply/4]).
-export([send_ack/2]).

-define(SETUP_CONSUMER_TIMEOUT, 10000).

%---------------------------------------------------------------------
% @doc does all the AMQP client preparations, namely: connection, channel,
% queue, exchange, binding.
-spec start(#rses{}) -> {ok, #conn{}}.

start(Rses) ->
    Host = Rses#rses.host,
    User = Rses#rses.user,
    Password = Rses#rses.password,
    Vhost = Rses#rses.vhost,
    {ok, Connection} = amqp_connection:start(network, #amqp_params{
        username = User,
        password = Password,
        host = Host,
        virtual_host = Vhost
        }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Access = #'access.request'{realm = Vhost,
        exclusive = false,
        passive = true,
        active = true,
        write = true,
        read = true},
    #'access.request_ok'{ticket = Ticket} = amqp_channel:call(Channel, Access),

    Q = Rses#rses.queue,
    X = Rses#rses.exchange,
    Xtype = Rses#rses.exchange_type,
    BindKey = Rses#rses.routing_key,

    QueueDeclare = #'queue.declare'{ticket = Ticket, queue = Q,
        passive = false, durable = false,
        exclusive = false, auto_delete = false,
        nowait = false, arguments = []},
    #'queue.declare_ok'{queue = Q} = amqp_channel:call(Channel, QueueDeclare),
    ExchangeDeclare = #'exchange.declare'{ticket = Ticket,
        exchange = X, type= Xtype,
        passive = false, durable = false,
        auto_delete=false, internal = false,
        nowait = false, arguments = []},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),
    QueueBind = #'queue.bind'{ticket = Ticket,
        queue = Q,
        exchange = X,
        routing_key = BindKey,
        nowait = false, arguments = []},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),
    {ok, ConsumerTag} = setup_consumer(Channel, Q),
    {ok, #conn{channel=Channel,
        connection=Connection,
        consumer_tag=ConsumerTag}
    }.
%---------------------------------------------------------------------
send_message(Channel, X, RoutingKey, Payload) ->
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}).
%---------------------------------------------------------------------
setup_consumer(Channel, Q) ->
    BasicConsume = #'basic.consume'{queue = Q, no_ack = false },
    #'basic.consume_ok'{consumer_tag = ConsumerTag}
        = amqp_channel:subscribe(Channel, BasicConsume, self()),
    receive
        #'basic.consume_ok'{consumer_tag = ConsumerTag} ->
            {ok, ConsumerTag}
    after ?SETUP_CONSUMER_TIMEOUT ->
        {error, setup_consumer_timeout}
    end
.
%---------------------------------------------------------------------
cancel_consumer(Channel, ConsumerTag) ->
    % After the consumer is finished interacting with the queue,
    % it can deregister itself
    BasicCancel = #'basic.cancel'{consumer_tag = ConsumerTag,
        nowait = false},
    #'basic.cancel_ok'{consumer_tag = ConsumerTag} =
        amqp_channel:call(Channel,BasicCancel)
.
%---------------------------------------------------------------------
teardown(#conn{connection = Connection,
        channel = Channel,
        consumer_tag = ConsumerTag}) ->
    cancel_consumer(Channel, ConsumerTag),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection)
.
%---------------------------------------------------------------------
send_reply(Channel, X, Rt_key, Payload) ->
    io:format("send_reply rt, payload:~n~p~n~p~n", [Rt_key, Payload]),
    send_message(Channel, X, Rt_key, Payload)
.
%---------------------------------------------------------------------
% @doc sends acknowledge for AMQP message.
-spec send_ack(#conn{}, any()) -> any().

send_ack(Conn, Tag) ->
    Channel = Conn#conn.channel,
    amqp_channel:call(Channel, #'basic.ack'{delivery_tag = Tag}).
%-------------------------------------------------------------------
