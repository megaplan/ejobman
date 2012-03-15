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
-export([get_config_hdl/1]).
-export([get_config_child/1]).
-export([get_config_receiver/0]).
-export([get_config_stat/0]).
-export([fill_one_pool_config/1]).
-export([get_config_group_handler/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("estat.hrl").
-include("ejobman.hrl").
-include("ejobman_child.hrl").
-include("group_handler.hrl").
-include("receiver.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc fills config for group handler for the particular group
%% @since 2011-12-30 15:56
%%
-spec get_config_group_handler(list()) -> #egh{}.

get_config_group_handler(List) ->
    Gh = proplists:get_value(group_handler, List, []),
    Max = case proplists:get_value(max_children, List) of
              N when is_integer(N) andalso N >= 0 ->
                  N;
              _ ->
                  proplists:get_value(max_children, Gh)
          end,
    Gid = case proplists:get_value(group, List) of
              undefined ->
                  erlang:error(undefined_group, List);
              Val ->
                  Val
          end,
    #egh{
          http_connect_timeout = proplists:get_value(http_connect_timeout,
                                                     Gh, ?HTTP_CONNECT_TIMEOUT),
          http_timeout = proplists:get_value(http_timeout, Gh, ?HTTP_TIMEOUT),
          schema_rewrite = proplists:get_value(schema_rewrite, Gh, []),
          url_rewrite = proplists:get_value(url_rewrite, Gh, []),
          id = proplists:get_value(id, List),
          debug = proplists:get_value(debug, Gh, []),
          group = Gid,
          max = Max
        }.

%%-----------------------------------------------------------------------------
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
        gh_pid = proplists:get_value(gh_pid, List),
        tag = proplists:get_value(tag, List),
        group = proplists:get_value(group, List),
        from = proplists:get_value(from, List),
        method = proplists:get_value(method, List, <<>>),
        url = proplists:get_value(url, List, <<>>),
        host = proplists:get_value(host, List),
        ip = proplists:get_value(ip, List),
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
    C = #ejm{
            ch_data = dict:new()
        },
    get_config_hdl(C).

-spec get_config_hdl(#ejm{}) -> #ejm{}.

get_config_hdl(Src) ->
    List = get_config_list(),
    fill_ejm_handler_config(List, Src).

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file for receiver, fills in ejm record with configured
%% values
%% @since 2011-07-15 13:19
%%
-spec get_config_receiver() -> #ejr{}.

get_config_receiver() ->
    List = get_config_list(),
    fill_config_receiver(List).

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file for stat, fills in est record with configured
%% values
%% @since 2011-12-20 13:19
%%
-spec get_config_stat() -> #est{}.

get_config_stat() ->
    List = get_config_list(),
    fill_config_stat(List).

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
        temp_rt_key_for_group = proplists:get_value(temp_rt_key_for_group,
                                                    List),
        debug = proplists:get_value(debug, List, []),
        log = proplists:get_value(log, List),
        pid_file = proplists:get_value(pid_file, List)
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc gets data from the list of key-value tuples and stores it into
%% ejr record
%% @since 2011-12-20 13:22
%%
-spec fill_config_stat(list()) -> #est{}.

fill_config_stat(All_list) ->
    List = proplists:get_value(estat, All_list, []),
    #est{
        % amount and time for last jobs
        stat_limit_n = proplists:get_value(stat_limit_n, List, ?STAT_LIMIT_N),
        stat_limit_t = proplists:get_value(stat_limit_t, List, ?STAT_LIMIT_T),
        % time limit for working/queued counters
        stat_limit_cnt_h = proplists:get_value(stat_limit_cnt_h, List,
                                             ?STAT_LIMIT_CT_H),
        stat_limit_cnt_m = proplists:get_value(stat_limit_cnt_m, List,
                                             ?STAT_LIMIT_CT_M),

        %log_procs_interval = proplists:get_value(log_procs_interval, List,
        %                                         ?LOG_PROCS_INTERVAL),
        %rt_info_file = proplists:get_value(rt_info_file, List),
        %rotate_interval = proplists:get_value(rotate_interval, List, 'hour'),
        debug = proplists:get_value(debug, List, []),
        %storage_base = proplists:get_value(storage_base, List, ?STAT_STORAGE),
        %keep_time = proplists:get_value(keep_time, List, ?STAT_KEEP_TIME),
        clean_interval = proplists:get_value(clean_interval, List,
                                             ?STAT_CLEAN_INTERVAL)
        %flush_number = proplists:get_value(flush_number, List,
        %                                     ?STAT_FLUSH_NUMBER)
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a handler config
%%
-spec fill_ejm_handler_config(list(), #ejm{}) -> #ejm{}.

fill_ejm_handler_config(List, Src) ->
    Gh_list = proplists:get_value(group_handler, List, []),
    Hdl_list = proplists:get_value(handler, List, []),
    Src#ejm{
        group_handler = Gh_list,
        job_groups = fill_job_groups(Gh_list),
        max_children = proplists:get_value(max_children, Gh_list, 3),
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
    C = fill_ejm_handler_config(Config, #ejm{}),
    C2 = [[{src_url,"192.168.9.183"},
         {dst_host,"promo.megaplan.kulikov"}],
        [{src_url,"promo.megaplan.kulikov"},
         {dst_url,"192.168.9.183"},
         {dst_host,"promo.megaplan.kulikov"}],
        [{src_type,regex},
         {src_url,[49,50,55,46,48,46,48,46,127,43]},
         {dst_url,"127.0.0.1"},
         {dst_host,"host3.localdomain"}]],
    %?assert(C#ejm.url_rewrite =:= C2)
    ?assert(false and C =:= C2) % url_rewrite moved to other record
.

%%-----------------------------------------------------------------------------
get_test_config_2() ->
[

].

%%-----------------------------------------------------------------------------
fill_ejm_handler_config_test() ->
    Config = get_test_config_2(),
    C = fill_ejm_handler_config(Config, #ejm{}),
    C2 = #ejm{
    },
    ?assert(C =:= C2).

-endif.
%%-----------------------------------------------------------------------------
