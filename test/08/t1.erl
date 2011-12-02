% Test for limits based on rabbit-mq queueing/limiting.
% Group max_children limit test.
% Test for mass requests with random id and group. Server must be configured
% to limit amount of children for different groups.
% WWW server must work reasonably slow to allow ejobman do limit.

-module(t1).
-compile(export_all).
%% erlc -I ../../include -I ../../../amqp_client/include t1.erl
-include_lib("amqp_client.hrl").

-include("../../include/receiver.hrl").
-include("../../include/job.hrl").
-include("../../include/rabbit_session.hrl").

-define(VSN, 6).
-define(EXCHANGE, <<"ejobman">>).
-define(RT_KEY, <<"new">>).

vsn() ->
    ?VSN.

conn() ->
    St = gen_server:call(ejobman_receiver, status),
    Conn = St#ejr.conn,
    Conn.

r() ->
    r(20).

r(Max) ->
    Conn = conn(),
    F = fun(_) -> spawn(?MODULE, r2, [Conn]) end,
    Pids = lists:map(F, lists:seq(1, Max)),
    St = gen_server:call(ejobman_handler, status),
    timer:sleep(300),
    St2 = gen_server:call(ejobman_handler, status),
    {Pids, St, St2}.

r2(Conn) ->
    Job = t(),
    Res = send_job(Conn, Job),
    error_logger:info_report({?MODULE, r2_res, Job, Res}).

send_job(#conn{channel=Channel}, Payload) ->
    send_message(Channel, ?EXCHANGE, ?RT_KEY, Payload).

send_message(Channel, X, RoutingKey, Payload) ->
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    Props = #'P_basic'{
        delivery_mode = 2 % make message persistent
    },
    Msg = #amqp_msg{payload = Payload, props = Props},
    amqp_channel:cast(Channel, Publish, Msg).

t() ->
    D = make_job(),
    J = mochijson2:encode(D),
    Json_b = iolist_to_binary(J),
    Json_b.

make_group(R) ->
    if  R < 10 ->
            undefined;
        R < 80 ->
            Rnd = crypto:rand_uniform(0, 5),
            R_n = list_to_binary(integer_to_list(Rnd)),
            <<"group_", R_n/binary>>;
        true ->
            default
    end.

make_job() ->
    crypto:start(),
    R = crypto:rand_uniform(0, 100),
    s(R).

make_url(N) ->
    Base = <<"http://localhost:8184/test_group_limit.yaws?new_id=">>,
    Url_n = list_to_binary(integer_to_list(N)),
    Url = <<Base/binary, Url_n/binary>>,
    Url.

s(R) ->
    [{<<"type">>,<<"rest">>},
     {<<"info">>,
          [{<<"group">>, make_group(R)},
           {<<"method">>, <<"get">>},
           {<<"url">>, make_url(R)},
           {<<"params">>,
                [{<<"orderId">>, R},
                 {<<"data">>,
                      [{<<"name">>,<<"Yourname">>},
                       {<<"time">>,
                        <<"Fri, 2 Dec 2011 17:25:54 +0400">>
                       }
                      ]
                 }
                ]
           }
          ]
     }
    ]
.

