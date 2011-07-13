%%%-----------------------------------------------------------------
%%% ejobman functions related to config file read, config processing
%%%-----------------------------------------------------------------
-module(ejobman_conf).
-export([get_config/1]).
-include("ejobman.hrl").
%-------------------------------------------------------------------
% @doc reads config file, fill ejm record with configured values
-spec get_config(string()) -> #ejm{}.

get_config(File) ->
	List = misc_conf:read_config(File),
	fill_config(List).
%-------------------------------------------------------------------
% @doc gets data from the list of key-value tuples and stores it into
% ejm record
-spec fill_config(list()) -> #ejm{}.

fill_config(List) ->
	#ejm{}.
%-------------------------------------------------------------------
