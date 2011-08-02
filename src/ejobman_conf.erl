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
-export([get_config_child/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").

%%%----------------------------------------------------------------------------
%%% api
%%%----------------------------------------------------------------------------
%%
%% @doc fills in the child config with values from input list.
%% @since 2011-07-15
%%
-spec get_config_child(list()) -> #child{}.

get_config_child(List) ->
    #child{
        name = proplists:get_value(name, List),
        id = proplists:get_value(id, List),
        duration = proplists:get_value(duration, List, 86400000),
        from = proplists:get_value(from, List),
        method = proplists:get_value(method, List, <<>>),
        url = proplists:get_value(url, List, <<>>),
        debug = proplists:get_value(debug, List, [])
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file, fills in ejm record with configured values
%% @since 2011-07-15
%%
-spec get_config_hdl(string()) -> #ejm{}.

get_config_hdl(Default) ->
    List = get_config_list(Default),
    Worker_list = proplists:get_value(worker, List, []),
    Hdl_list = proplists:get_value(handler, List, []),
    #ejm{
        worker_config = Worker_list,
        workers = [],
        w_queue = queue:new(),
        w_duration = proplists:get_value(worker_duration, Hdl_list, 86400000),
        min_workers = proplists:get_value(min_workers, Hdl_list, 2),
        max_workers = proplists:get_value(max_workers, Hdl_list, 255),
        ch_data = [],
        ch_queue = queue:new(),
        max_children = proplists:get_value(max_children, Hdl_list, 32767),
        debug = proplists:get_value(debug, Hdl_list, [])
    }.

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

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
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
-endif.
