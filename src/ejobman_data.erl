%%%
%%% ejobman_data: ejobman data functions
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
%%% @doc ejobman data related functions
%%%

-module(ejobman_data).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([get_type/1, get_rest_info/1, get_method/1, get_url/1]).
-export([get_time/1, get_params/1]).

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
-spec get_rest_info(any()) -> any().
%%
%% @doc Extracts value for "rest_info" item from deserialized json structure
%% @since 2011-07-15
%%
get_rest_info(Data) ->
    get_value(Data, <<"rest_info">>).

%%-----------------------------------------------------------------------------
-spec get_method(any()) -> any().
%%
%% @doc Extracts value for "method" item from rest_info json structure
%% @since 2011-07-15
%%
get_method(Data) ->
    get_value(Data, <<"method">>).

%%-----------------------------------------------------------------------------
-spec get_url(any()) -> any().
%%
%% @doc Extracts value for "url" item from rest_info json structure
%% @since 2011-07-15
%%
get_url(Data) ->
    get_value(Data, <<"url">>).

%%-----------------------------------------------------------------------------
%%
%% @doc Extracts value for "params" item from rest_info json structure
%% @since 2011-08-04 14:09
%%
-spec get_params(any()) -> list().

get_params(Data) ->
    case get_value(Data, <<"params">>) of
        {struct, List} when is_list(List) ->
            List;
        _ ->
            []
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc Extracts value for "run_time" item from rest_info json structure
%% @since 2011-08-02 13:56
%%
-spec get_time(any()) -> any().

get_time(Data) ->
    get_value(Data, <<"run_time">>).

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

%%-----------------------------------------------------------------------------
