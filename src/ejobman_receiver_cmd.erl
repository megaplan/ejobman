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

-export([store_rabbit_cmd/4]).
-export([store_consumer_tag/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("receiver.hrl").
%-include("ejobman.hrl").
-include("job.hrl").
-include("rabbit_session.hrl").

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

%-define(HTTP_TIMEOUT, 15000).

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
%% @doc sends received command to a command handler. Returns nothing actually.
%% @since 2011-07-15
%%
-spec store_rabbit_cmd(#ejr{}, binary(), reference(), binary()) -> #ejr{}.

store_rabbit_cmd(State, Tag, Ref, Bin) ->
    mpln_p_debug:pr({?MODULE, 'store_rabbit_cmd json', ?LINE, Ref, Bin},
        State#ejr.debug, msg, 4),
    case catch mochijson2:decode(Bin) of
        {'EXIT', Reason} ->
            mpln_p_debug:pr({?MODULE, 'store_rabbit_cmd error',
                ?LINE, Ref, Reason}, State#ejr.debug, run, 2),
            ejobman_rb:send_ack(State#ejr.conn, Tag);
        Data ->
            mpln_p_debug:pr({?MODULE, 'store_rabbit_cmd json dat',
                ?LINE, Ref, Data}, State#ejr.debug, json, 3),
            Type = ejobman_data:get_type(Data),
            send_to_estat(Ref, Data),
            proceed_cmd_type(State, Type, Tag, Ref, Data)
    end,
    State.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc removes auth data from data and sends the rest to ejobman_stat
%%
send_to_estat(Ref, Data) ->
    Info = ejobman_data:get_rest_info(Data),
    Clean = ejobman_data:del_auth_info(Info),
    ejobman_stat:add(Ref, 'from_rabbit', {'rest_info', Clean}).

%%-----------------------------------------------------------------------------
%%
%% @doc calls ejobman_handler with received command info
%%
-spec proceed_cmd_type(#ejr{}, binary(), binary(), reference(), any()) -> ok.

proceed_cmd_type(State, <<"rest">>, Tag, Ref, Data) ->
    Job = make_job(Tag, Ref, Data),
    mpln_p_debug:pr({?MODULE, 'proceed_cmd_type job_id', ?LINE, Job#job.id},
        State#ejr.debug, job, 2),
    mpln_p_debug:pr({?MODULE, 'proceed_cmd_type job', ?LINE, Job},
        State#ejr.debug, job, 4),
    Res = (catch ejobman_handler:cmd(Job)),
    mpln_p_debug:pr({?MODULE, 'proceed_cmd_type res', ?LINE, Res},
        State#ejr.debug, run, 5);

proceed_cmd_type(State, Other, Tag, Ref, _Data) ->
    mpln_p_debug:pr({?MODULE, 'proceed_cmd_type other', ?LINE, Ref, Other},
                    State#ejr.debug, run, 2),
    ejobman_rb:send_ack(State#ejr.conn, Tag).

%%-----------------------------------------------------------------------------
%%
%% @doc fills in a #job record
%%
-spec make_job(binary(), reference(), any()) -> #job{}.

make_job(Tag, Ref, Data) ->
    Info = ejobman_data:get_rest_info(Data),
    A = make_job_auth(Info),
    Method = ejobman_data:get_method(Info),
    Url = ejobman_data:get_url(Info),
    Host = ejobman_data:get_host(Info),

    Params = ejobman_data:get_params(Info),
    Flat_params = mpln_misc_web:flatten(Params, true),

    Group = ejobman_data:get_group(Info),
    T_data = ejobman_data:get_time(Info),
    T = make_time(T_data),
    A#job{
        id = Ref,
        tag = Tag,
        method = Method,
        url = Url,
        host = Host,
        params = Flat_params,
        group = Group,
        run_time = T
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a #job record with auth data filled in
%%
-spec make_job_auth(any()) -> #job{}.

make_job_auth(Info) ->
    Auth = ejobman_data:get_auth_info(Info),
    Type = ejobman_data:get_auth_type(Auth),
    Str = mpln_misc_web:make_string(Type),
    #job{
        auth = fill_auth_data(Str, Auth)
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a filled #auth record
%%
-spec fill_auth_data(any(), any()) -> #auth{}.

fill_auth_data("megaplan", Auth) ->
    F = fun ({<<"type">>, _}) -> false;
            ({"type", _})     -> false;
            ({_, _})          -> true;
            (_)               -> false
    end,
    List = ejobman_data:get_auth_data_list(Auth),
    Data = lists:filter(F, List),
    {A, S} = ejobman_data:get_auth_keys(Auth),
    #auth{type='megaplan', data = Data, auth_key = A, secret_key = S};
fill_auth_data(_, Auth) ->
    User = ejobman_data:get_auth_user(Auth),
    Pass = ejobman_data:get_auth_password(Auth),
    #auth{type='basic', user = User, password = Pass}.

%%-----------------------------------------------------------------------------
%%
%% @doc fills in #rt record
%%
-spec make_time(any()) -> #rt{}.

make_time(Data) ->
    #rt{}
.
%%-----------------------------------------------------------------------------
