%%%
%%% ejobman_conf: functions for config
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
%%% @doc ejobman functions related to config file read, config processing
%%%

-module(ejobman_conf).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([get_config_hdl/0]).
-export([get_config_child/1]).
-export([get_config_receiver/0]).
-export([fill_one_pool_config/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").
-include("receiver.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc fills in the child config with values from input list.
%% @since 2011-07-15
%%
-spec get_config_child(list()) -> #child{}.

get_config_child(List) ->
    #child{
        http_connect_timeout = proplists:get_value(http_connect_timeout, List,
            ?HTTP_CONNECT_TIMEOUT),
        http_timeout = proplists:get_value(http_timeout, List, ?HTTP_TIMEOUT),
        schema_rewrite = proplists:get_value(schema_rewrite, List, []),
        url_rewrite = proplists:get_value(url_rewrite, List, []),
        name = proplists:get_value(name, List),
        id = proplists:get_value(id, List),
        tag = proplists:get_value(tag, List),
        group = proplists:get_value(group, List),
        from = proplists:get_value(from, List),
        method = proplists:get_value(method, List, <<>>),
        url = proplists:get_value(url, List, <<>>),
        host = proplists:get_value(host, List),
        auth = proplists:get_value(auth, List),
        params = proplists:get_value(params, List, []),
        debug = proplists:get_value(debug, List, [])
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file, fills in ejm record with configured values
%% @since 2011-07-15
%%
-spec get_config_hdl() -> #ejm{}.

get_config_hdl() ->
    List = get_config_list(),
    fill_ejm_handler_config(List).

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file for receiver, fills in ejm record with configured
%% values
%% @since 2011-07-15
%%
-spec get_config_receiver() -> #ejr{}.

get_config_receiver() ->
    List = get_config_list(),
    fill_config_receiver(List).

%%-----------------------------------------------------------------------------
%%
%% @doc fills config for one pool
%%
fill_one_pool_config(List) ->
    #pool{
        id = proplists:get_value(id, List),
        worker_config = proplists:get_value(worker, List, []),
        workers = [],
        w_queue = queue:new(),
        w_duration = get_worker_duration(List),
        restart_policy = proplists:get_value(restart_policy, List),
        restart_delay = proplists:get_value(restart_delay, List, 10),
        min_workers = proplists:get_value(min_workers, List, 2)
    }.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc gets worker duration from the list
%% @since 2011-08-25 17:50
%%
get_worker_duration(List) ->
    case proplists:get_value(worker_duration, List, 86400) of
        Val when is_integer(Val), Val > 0 ->
            Val;
        _ ->
            0
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc fetches the configuration from environment
%% @since 2011-08-01 17:01
%%
-spec get_config_list() -> list().

get_config_list() ->
    application:get_all_env('ejobman').

%%-----------------------------------------------------------------------------
%%
%% @doc gets data from the list of key-value tuples and stores it into
%% ejr record
%% @since 2011-07-15
%%
-spec fill_config_receiver(list()) -> #ejr{}.

fill_config_receiver(List) ->
    Rses = ejobman_conf_rabbit:stuff_rabbit_with(List),
    #ejr{
        rses = Rses,
        debug = proplists:get_value(debug, List, []),
        log = proplists:get_value(log, List),
        pid_file = proplists:get_value(pid_file, List)
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a handler config
%%
-spec fill_ejm_handler_config(list()) -> #ejm{}.

fill_ejm_handler_config(List) ->
    Hdl_list = proplists:get_value(handler, List, []),
    #ejm{
        job_groups = fill_job_groups(Hdl_list),
        ch_data = dict:new(),
        ch_queues = dict:new(),
        job_log = proplists:get_value(job_log, Hdl_list),
        job_log_last = calendar:local_time(),
        job_log_rotate = proplists:get_value(job_log_rotate, Hdl_list),
        http_connect_timeout = proplists:get_value(http_connect_timeout, Hdl_list, ?HTTP_CONNECT_TIMEOUT),
        http_timeout = proplists:get_value(http_timeout, Hdl_list, ?HTTP_TIMEOUT),
        schema_rewrite = proplists:get_value(schema_rewrite, Hdl_list, []),
        url_rewrite = proplists:get_value(url_rewrite, Hdl_list, []),
        max_children = proplists:get_value(max_children, Hdl_list, 32767),
        debug = proplists:get_value(debug, Hdl_list, [])
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc converts list of proplists with group data to list of #jgroup{}
%%
-spec fill_job_groups([list()]) -> [#jgroup{}].

fill_job_groups(List) ->
    L2 = proplists:get_value(job_groups, List, []),
    F = fun(Small) ->
        Id = proplists:get_value(name, Small),
        Max = proplists:get_value(max_children, Small),
        #jgroup{id=Id, max_children=Max}
    end,
    lists:map(F, L2).

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
fill_config_test() ->
    #ejr{rses=_, debug=[], log=?LOG} = fill_config_receiver([]),
    #ejr{rses=_, debug=[{info, 5}, {run, 2}], log=?LOG} =
    fill_config_receiver([
        {debug, [{info, 5}, {run, 2}]}
        ]).

%%-----------------------------------------------------------------------------
get_test_config() ->
[
{handler, [
    {url_rewrite, [
        [
            {src_url, "192.168.9.183"},
            {dst_host, "promo.megaplan.kulikov"}
        ],
        [
            {src_url, "promo.megaplan.kulikov"},
            {dst_url, "192.168.9.183"},
            {dst_host, "promo.megaplan.kulikov"}
        ],
        [
            {src_type, regex},
            {src_url, "127\.0\.0\.\d+"},
            {dst_url, "127.0.0.1"},
            {dst_host, "host3.localdomain"}
        ]
    ]},
    {max_children, 7}, % to process short command
    {debug,
        [
            {config, 4},
            {store, 0},
            {get, 4},
            {run, 5},
            {http, 0},
            {ets, 3}
        ]
    }]},
{log, "log/e"}
].

%%-----------------------------------------------------------------------------
fill_ejm_config_test() ->
    Config = get_test_config(),
    C = fill_ejm_handler_config(Config),
    C2 = [[{src_url,"192.168.9.183"},
         {dst_host,"promo.megaplan.kulikov"}],
        [{src_url,"promo.megaplan.kulikov"},
         {dst_url,"192.168.9.183"},
         {dst_host,"promo.megaplan.kulikov"}],
        [{src_type,regex},
         {src_url,[49,50,55,46,48,46,48,46,127,43]},
         {dst_url,"127.0.0.1"},
         {dst_host,"host3.localdomain"}]],
    ?assert(C#ejm.url_rewrite =:= C2)
.

%%-----------------------------------------------------------------------------
get_test_config_2() ->
[

].

%%-----------------------------------------------------------------------------
fill_ejm_handler_config_test() ->
    Config = get_test_config_2(),
    C = fill_ejm_handler_config(Config),
    C2 = #ejm{
    },
    ?assert(C =:= C2).

-endif.
%%-----------------------------------------------------------------------------
