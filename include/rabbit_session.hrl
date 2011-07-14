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
    'routing_key' = <<"test_topless">>,
    'timeout' = 15,
    'step_timeout' = 0.1,
    'sep' = <<"=~">> % sep MUST have size of 2 bytes as it is matched so
                     % in ejobman_receiver_cmd:store_rabbit_cmd
}).

-record(conn, {
    'channel' = false,
    'connection' = false,
    'consumer_tag' = false
}).

-endif.
