-ifndef(rabbit_session).
-define(rabbit_session, true).

-record(rses, {
    'host' = "127.0.0.1",
    'port' = 5672,
    'user' = <<"guest">>,
    'password' = <<"guest">>,
    'vhost' = <<"/">>,
    'exchange' = <<"test_e">>,
    'exchange_type' = <<"topic">>,
    'queue' = <<"test_q">>,
    'routing_key' = <<"test_topless">>
}).

-record(conn, {
    'channel' = false,
    'connection' = false,
    'consumer_tag' = false
}).

-endif.
