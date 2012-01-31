%%%
%%% ejobman_receiver_cmd: payload handling
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
%%% @doc functions that do real handling of the payload received via AMQP
%%%

-module(ejobman_receiver_cmd).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([store_consumer_tag/2]).
-export([get_conn_params/1]).
-export([push_message/4]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("receiver.hrl").
-include("job.hrl").
-include("rabbit_session.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc stores consumer tag if it's undefined. Returns updated state
%% @since 2011-12-02 14:19
%%
store_consumer_tag(#ejr{conn=#conn{consumer_tag=undefined} = Conn} = State,
                   Tag) ->
    mpln_p_debug:pr({?MODULE, 'consumer tag', ?LINE, Tag},
                    State#ejr.debug, run, 3),
    New = Conn#conn{consumer_tag=Tag},
    State#ejr{conn=New};

store_consumer_tag(#ejr{conn=#conn{consumer_tag=Tag}} = State, Tag) ->
    mpln_p_debug:pr({?MODULE, 'confirmation of consumer tag', ?LINE},
                    State#ejr.debug, run, 2),
    State;

store_consumer_tag(State, _Tag) ->
    mpln_p_debug:pr({?MODULE, 'unknown consumer tag', ?LINE},
                    State#ejr.debug, run, 2),
    State.

%%-----------------------------------------------------------------------------
%%
%% @doc pushes the received message to an appropriate exchange
%% @since 2012-01-10 18:01
%%
-spec push_message(#ejr{}, binary(), reference(), binary()) -> ok.

push_message(#ejr{conn=Conn} = St, Gid, Ref, Payload) ->
    case find_exchange(St, Gid, Payload) of
        undefined ->
            % receiver got a message, but group handlers have not reported
            % yet
            mpln_p_debug:pr({?MODULE, 'push_message undefined', ?LINE,
                             Ref, Gid, Payload}, St#ejr.debug, msg, 1),
            ok;
        {Ex, Rkey} ->
            Bref = mpln_misc_web:make_term_binary(Ref),
            mpln_p_debug:pr({?MODULE, 'push_message', ?LINE,
                             Conn#conn.channel, Gid, Ex, Rkey, Payload, Bref},
                            St#ejr.debug, msg, 4),
            ejobman_rb:send_dur_message(Conn#conn.channel,
                                        Ex, Rkey, Payload, Bref)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc returns vhost and connection parameters
%% @since 2012-01-10 14:55
%%
-spec get_conn_params(#ejr{}) -> {binary(), #conn{}}.

get_conn_params(#ejr{conn=Conn, rses=Rses}) ->
    {Rses#rses.vhost, Conn}.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc returns either an exchange for the given routing key or
%% an exchange for the default key, or undefined otherwise
%%
-spec find_exchange(#ejr{}, binary(), binary()) ->
                           undefined
                               | { binary(), binary()}.

find_exchange(#ejr{temp_rt_key_for_group=Key} = St, Key, Bin) ->
    % this brunch is a temporary stub. When the input rt key is the same as
    % the configured temp_rt_key then the real rt key should be extracted
    % from the group field of payload
    case catch mochijson2:decode(Bin) of
        {'EXIT', Reason} ->
            mpln_p_debug:pr({?MODULE, 'find_exchange error', ?LINE, Reason},
                            St#ejr.debug, run, 2),
            find_exchange2(St, Key, false);
        Data ->
            mpln_p_debug:pr({?MODULE, 'find_exchange json dat', ?LINE, Data},
                            St#ejr.debug, json, 3),
            Type = ejobman_data:get_type(Data),
            proceed_with_type(St, Key, Type, Data)
    end;

find_exchange(St, Key, _) ->
    find_exchange2(St, Key, false).

find_exchange2(#ejr{groups=Groups} = St, Key, Last) ->
    mpln_p_debug:pr({?MODULE, 'find_exchange2', ?LINE, Key, Last},
                    St#ejr.debug, run, 4),
    case lists:keyfind(Key, 1, Groups) of
        false when Last == true ->
            undefined;
        false ->
            find_exchange2(St, ?GID_DEFAULT, true);
        {_Key, Exchange, Bind_key} ->
            {Exchange, Bind_key}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc extracts group data from payload. Temporary solution.
%%
proceed_with_type(St, _Key, <<"rest">>, Data) ->
    Info = ejobman_data:get_rest_info(Data),
    Group = ejobman_data:get_group(Info),
    mpln_p_debug:pr({?MODULE, 'proceed_with_type rest', ?LINE, Group},
                    St#ejr.debug, run, 4),
    find_exchange2(St, Group, false);
    
proceed_with_type(St, Key, _, _) ->
    find_exchange2(St, Key, false).

%%-----------------------------------------------------------------------------
