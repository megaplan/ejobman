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
%%% @doc AMQP client config functions
%%%

-module(ejobman_conf_rabbit).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([stuff_rabbit_with/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("rabbit_session.hrl").

%%%----------------------------------------------------------------------------
%%% api
%%%----------------------------------------------------------------------------
%%
%% @doc fills in an rses record with rabbit connection parameters.
%% @since 2011-07-15
%%
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
%%-----------------------------------------------------------------------------
