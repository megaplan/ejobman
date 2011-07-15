%%%-----------------------------------------------------------------
%%% JSON related functions
%%%-----------------------------------------------------------------
-module(misc_json).
-export([get_type/1, get_job_info/1, get_method/1, get_url/1]).
%-------------------------------------------------------------------
% @doc extracts value for tagged item from deserialized json structure
-spec get_value(any(), binary()) -> any().

get_value({struct, List}, Tag) ->
    case catch proplists:get_value(Tag, List) of
        {'EXIT', _} ->
            undefined;
        undefined ->
            undefined;
        Type ->
            Type
    end.
%-------------------------------------------------------------------
% @doc extracts value for "type" item from deserialized json structure
-spec get_type(any()) -> any().

get_type(Data) ->
    get_value(Data, <<"type">>).
%-------------------------------------------------------------------
% @doc extracts value for "job_info" item from deserialized json structure
-spec get_job_info(any()) -> any().

get_job_info(Data) ->
    get_value(Data, <<"job_info">>).
%-------------------------------------------------------------------
% @doc extracts value for "method" item from job_info json structure
-spec get_method(any()) -> any().

get_method(Data) ->
    get_value(Data, <<"method">>).
%-------------------------------------------------------------------
% @doc extracts value for "url" item from job_info json structure
-spec get_url(any()) -> any().

get_url(Data) ->
    get_value(Data, <<"url">>).
%-------------------------------------------------------------------
