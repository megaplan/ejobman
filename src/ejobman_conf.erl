%%%-----------------------------------------------------------------
%%% ejobman functions related to config file read, config processing
%%%-----------------------------------------------------------------
-module(ejobman_conf).
-export([get_config/1]).
-export([get_config_hdl/1]).
-export([get_config_child/1]).
-include("ejobman.hrl").
%-------------------------------------------------------------------
% @doc fills in the child config with values from input list.
-spec get_config_child(list()) -> #child{}.

get_config_child(List) ->
    #child{
        from = proplists:get_value(from, List),
        cmd = proplists:get_value(cmd, List, <<>>),
        debug = proplists:get_value(debug, List, [])
    }.
%-------------------------------------------------------------------
% @doc reads config file, fills in ejm record with configured values
-spec get_config_hdl(string()) -> #ejm{}.

get_config_hdl(File) ->
    List = misc_conf:read_config(File),
    Hdl_list = proplists:get_value(handler, List, []),
    #ejm{
        debug = proplists:get_value(debug, Hdl_list, [])
    }.
%-------------------------------------------------------------------
% @doc reads config file, fills in ejm record with configured values
-spec get_config(string()) -> #ejm{}.

get_config(File) ->
    List = misc_conf:read_config(File),
    fill_config(List).
%-------------------------------------------------------------------
% @doc gets data from the list of key-value tuples and stores it into
% ejm record
-spec fill_config(list()) -> #ejm{}.

fill_config(List) ->
    Rses = ejobman_conf_rabbit:stuff_rabbit_with(List),
    #ejm{
        rses = Rses,
        debug = proplists:get_value(debug, List, []),
        log = proplists:get_value(log, List, ?LOG)
    }.
%-------------------------------------------------------------------
