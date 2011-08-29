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

-export([get_config/1]).
-export([get_config_hdl/1]).
-export([get_config_worker/1]).
-export([get_config_child/1]).
-export([fill_one_pool_config/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").

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
        url_rewrite = proplists:get_value(url_rewrite, List, []),
        name = proplists:get_value(name, List),
        id = proplists:get_value(id, List),
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
%% @since 2011-08-29 14:09
%%
-spec get_config_worker(string()) -> #ejm{}.

get_config_worker(Default) ->
    List = get_config_list(Default),
    fill_ejm_worker_config(List).

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file, fills in ejm record with configured values
%% @since 2011-07-15
%%
-spec get_config_hdl(string()) -> #ejm{}.

get_config_hdl(Default) ->
    List = get_config_list(Default),
    fill_ejm_handler_config(List).

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file for receiver, fills in ejm record with configured
%% values
%% @since 2011-07-15
%%
-spec get_config(string()) -> #ejm{}.

get_config(Default) ->
    List = get_config_list(Default),
    fill_config(List).

%%-----------------------------------------------------------------------------
%%
%% @doc gets data from the list of key-value tuples and stores it into
%% ejm record
%% @since 2011-07-15
%%
-spec fill_config(list()) -> #ejm{}.

fill_config(List) ->
    Rses = ejobman_conf_rabbit:stuff_rabbit_with(List),
    #ejm{
        rses = Rses,
        debug = proplists:get_value(debug, List, []),
        log = proplists:get_value(log, List, ?LOG)
    }.

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
%% @doc chooses either file from application config or default file and then
%% does read_config for that file
%% @since 2011-08-01 17:01
%%
-spec get_config_list(string()) -> list().

get_config_list(Default) ->
    case application:get_env('ejobman', 'CONFIG') of
        {ok, File} when is_list(File) ->
            mpln_misc_conf:read_config(File);
        {ok, A} when is_atom(A) ->
            File = atom_to_list(A),
            mpln_misc_conf:read_config(File);
        _ ->
            mpln_misc_conf:read_config(Default)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc fills configs for the pools defined
%%
fill_pools_config(List) ->
    Pools = proplists:get_value(pools, List, []),
    lists:map(fun fill_one_pool_config/1, Pools)
.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a worker config
%% @since 2011-08-29 14:15
%%
-spec fill_ejm_worker_config(list()) -> #ejm{}.

fill_ejm_worker_config(List) ->
    Hdl_list = proplists:get_value(ll_worker, List, []),
    Pools = fill_pools_config(Hdl_list),
    Web = fill_web_config(Hdl_list),
    Web#ejm{
        w_pools = Pools,
        debug = proplists:get_value(debug, Hdl_list, [])
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a handler config
%%
-spec fill_ejm_handler_config(list()) -> #ejm{}.

fill_ejm_handler_config(List) ->
    Hdl_list = proplists:get_value(handler, List, []),
    #ejm{
        ch_data = [],
        ch_queue = queue:new(),
        url_rewrite = proplists:get_value(url_rewrite, Hdl_list, []),
        max_children = proplists:get_value(max_children, Hdl_list, 32767),
        debug = proplists:get_value(debug, Hdl_list, [])
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc gets web server parameters and stores them in the config record
%%
fill_web_config(List) ->
    #ejm{
        web_server_opts = proplists:get_value(web_server_opts, List, [])
    }.

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
fill_config_test() ->
    #ejm{rses=_, debug=[], log=?LOG} = fill_config([]),
    #ejm{rses=_, debug=[{info, 5}, {run, 2}], log=?LOG} =
    fill_config([
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

{pools, [
    [
        {id, p4},
        {min_workers, 1}, % long lasting workers
        {restart_policy, delay},
        {restart_delay, 10}, % sec. Delay before restarting the crashed worker
        {worker_duration, 601}, % seconds. Time before terminate
        {worker, [
            {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
            {debug,
                [
                    {run, 6},
                    {http, 1}
                ]
            }]}
    ],
    [
        {id, p1},
        {min_workers, 1}, % long lasting workers
        {restart_policy, delay},
        {restart_delay, 10}, % sec. Delay before restarting the crashed worker
        {worker_duration, 0}, % seconds. Time before terminate
        {worker, [
            {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
            {debug,
                [
                    {run, 6},
                    {http, 1}
                ]
            }]}
    ],
    [
        {id, p3},
        {min_workers, 1}, % long lasting workers
        {restart_policy, delay},
        {restart_delay, 10}, % sec. Delay before restarting the crashed worker
        {worker, [
            {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
            {debug,
                [
                    {run, 6},
                    {http, 1}
                ]
            }]}
    ],
    [
        {id, p2},
        {min_workers, 2}, % long lasting workers
        {worker_duration, -1}, % seconds. Time before terminate
        {worker, [
            {name, "/etc/erpher/workers/test.sh"},
            {debug,
                [
                    {run, 6},
                    {http, 1}
                ]
            }]}
    ]
]}

].

%%-----------------------------------------------------------------------------
fill_ejm_handler_config_test() ->
    Config = get_test_config_2(),
    C = fill_ejm_handler_config(Config),
    C2 = #ejm{
        w_pools = [
            #pool{
                w_queue = queue:new(),
                id=p4,
                min_workers=1,
                restart_policy=delay,
                restart_delay=10,
                w_duration=601,
                worker_config= [
                    {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
                    {debug,
                        [
                            {run, 6},
                            {http, 1}
                        ]
                    }]
            },
            #pool{
                w_queue = queue:new(),
                id=p1,
                min_workers=1,
                restart_policy=delay,
                restart_delay=10,
                w_duration=0,
                worker_config=[
                    {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
                    {debug,
                        [
                            {run, 6},
                            {http, 1}
                        ]
                    }]
            },
            #pool{
                w_queue = queue:new(),
                id=p3,
                min_workers=1,
                restart_policy=delay,
                restart_delay=10,
                worker_config=[
                    {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
                    {debug,
                        [
                            {run, 6},
                            {http, 1}
                        ]
                    }]
            },
            #pool{
                w_queue = queue:new(),
                id=p2,
                min_workers=2,
                w_duration=0,
                restart_delay=10,
                worker_config=[
                    {name, "/etc/erpher/workers/test.sh"},
                    {debug,
                        [
                            {run, 6},
                            {http, 1}
                        ]
                    }]
            }
        ]
    },
    ?assert(C#ejm.w_pools =:= C2#ejm.w_pools).

-endif.
%%-----------------------------------------------------------------------------
