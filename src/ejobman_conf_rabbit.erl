-module(ejobman_conf_rabbit).
-export([stuff_rabbit_with/1]).
-include("rabbit_session.hrl").
%-------------------------------------------------------------------
% @doc fills in rses record with rabbit connection parameters.
-spec stuff_rabbit_with(list()) -> #rses{}.

stuff_rabbit_with(List) ->
    R = proplists:get_value(rabbit, List, []),
    #rses{
        'host' = proplists:get_value(host, R, '127.0.0.1'),
        'port' = proplists:get_value(port, R, 5672),
        'user' = proplists:get_value(user, R, <<"guest">>),
        'password' = proplists:get_value(password, R, <<"guest">>),
        'vhost' = proplists:get_value(vhost, R, <<"/">>),
        'exchange' = proplists:get_value(exchange, R, <<"test_exch">>),
        'exchange_type' = proplists:get_value(exchange_type, R, <<"topic">>),
        'queue' = proplists:get_value(queue, R, <<"test_queue">>),
        'routing_key' = proplists:get_value(routing_key, R, <<"test_rt_key">>),
        'timeout' = proplists:get_value(timeout, R, 15),
        'step_timeout' = proplists:get_value(step_timeout, R, 0.1)
    }
.
%-------------------------------------------------------------------
