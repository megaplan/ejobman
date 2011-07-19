%%%
%%% mpln_misc_json: JSON related functions
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
%%% @doc JSON related functions
%%%

-module(mpln_misc_json).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([get_type/1, get_job_info/1, get_method/1, get_url/1]).

%%%----------------------------------------------------------------------------
%%% Public API
%%%----------------------------------------------------------------------------

-spec get_type(any()) -> any().
%%
%% @doc Extracts value for "type" item from deserialized json structure
%% @since 2011-07-15
%%
get_type(Data) ->
    get_value(Data, <<"type">>).
%%-----------------------------------------------------------------------------

-spec get_job_info(any()) -> any().
%%
%% @doc Extracts value for "job_info" item from deserialized json structure
%% @since 2011-07-15
%%
get_job_info(Data) ->
    get_value(Data, <<"job_info">>).
%%-----------------------------------------------------------------------------

-spec get_method(any()) -> any().
%%
%% @doc Extracts value for "method" item from job_info json structure
%% @since 2011-07-15
%%
get_method(Data) ->
    get_value(Data, <<"method">>).

-spec get_url(any()) -> any().
%%
%% @doc Extracts value for "url" item from job_info json structure
%% @since 2011-07-15
%%
get_url(Data) ->
    get_value(Data, <<"url">>).
%%-----------------------------------------------------------------------------

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

-spec get_value(any(), binary()) -> any().
%%
%% @doc Extracts value for tagged item from deserialized json structure
%%
get_value({struct, List}, Tag) ->
    case catch proplists:get_value(Tag, List) of
        {'EXIT', _} ->
            undefined;
        undefined ->
            undefined;
        Type ->
            Type
    end.
