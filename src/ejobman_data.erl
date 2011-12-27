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
-export([get_time/1, get_params/1, get_host/1]).
-export([get_auth_info/1, get_auth_user/1, get_auth_password/1]).
-export([get_auth_type/1, get_auth_data_list/1]).
-export([get_auth_keys/1]).
-export([get_group/1]).
-export([del_auth_info/1]).
-export([get_ip/1]).

%%%----------------------------------------------------------------------------
%%% Public API
%%%----------------------------------------------------------------------------
-spec get_group(any()) -> any().
%%
%% @doc Extracts value for "group" item from deserialized json structure
%% @since 2011-11-11 17:33
%%
get_group(Data) ->
    get_value(Data, <<"group">>).

%%-----------------------------------------------------------------------------
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
%% @doc Extracts value for "info" item from deserialized json structure
%% @since 2011-07-15
%%
get_rest_info(Data) ->
    get_value(Data, <<"info">>).

%%-----------------------------------------------------------------------------
-spec get_method(any()) -> any().
%%
%% @doc Extracts value for "method" item from info json structure
%% @since 2011-07-15
%%
get_method(Data) ->
    get_value(Data, <<"method">>).

%%-----------------------------------------------------------------------------
-spec get_url(any()) -> any().
%%
%% @doc Extracts value for "url" item from info json structure
%% @since 2011-07-15
%%
get_url(Data) ->
    get_value(Data, <<"url">>).

%%-----------------------------------------------------------------------------
-spec get_host(any()) -> any().
%%
%% @doc Extracts value for "host" item from info json structure
%% @since 2011-08-05 16:18
%%
get_host(Data) ->
    get_value(Data, <<"host">>).

%%-----------------------------------------------------------------------------
-spec get_ip(any()) -> any().
%%
%% @doc Extracts value for "ip" item from info json structure
%% @since 2011-12-27 16:02
%%
get_ip(Data) ->
    get_value(Data, <<"ip">>).

%%-----------------------------------------------------------------------------
%%
%% @doc Extracts value for "params" item from info json structure
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
%% @doc Extracts value for "run_time" item from info json structure
%% @since 2011-08-02 13:56
%%
-spec get_time(any()) -> any().

get_time(Data) ->
    get_value(Data, <<"run_time">>).

%%-----------------------------------------------------------------------------
%%
%% @doc cleans the data from the "auth_info" item
%% @since 2011-12-21 18:42
%%
-spec del_auth_info(any()) -> any().

del_auth_info({struct, Data}) ->
    del_auth_info(Data);

del_auth_info(Data) when is_list(Data) ->
    F = fun({<<"auth_info">>, _}) ->
                false;
           (_) ->
                true
        end,
    lists:filter(F, Data);

del_auth_info(_Data) ->
    undefined.

%%-----------------------------------------------------------------------------
%%
%% @doc Extracts value for "auth_info" item from deserialized json structure
%% @since 2011-07-15
%%
-spec get_auth_info(any()) -> any().

get_auth_info(Data) ->
    get_value(Data, <<"auth_info">>).

%%-----------------------------------------------------------------------------
%%
%% @doc Extracts value for "type" item from deserialized auth_info
%% json structure
%% @since 2011-08-10 18:19
%%
-spec get_auth_type(any()) -> any().

get_auth_type(Data) ->
    get_value(Data, <<"type">>).

%%-----------------------------------------------------------------------------
%%
%% @doc Extracts value for "user" item from deserialized auth_info
%% json structure
%% @since 2011-08-10 18:19
%%
-spec get_auth_user(any()) -> any().

get_auth_user(Data) ->
    get_value(Data, <<"user">>).

%%-----------------------------------------------------------------------------
%%
%% @doc Extracts value for "key" item from deserialized auth_info
%% json structure
%% @since 2011-08-10 18:19
%%
-spec get_auth_keys(any()) -> any().

get_auth_keys(Data) ->
    A = get_value(Data, <<"authKey">>),
    S = get_value(Data, <<"secretKey">>),
    {A, S}.

%%-----------------------------------------------------------------------------
%%
%% @doc Extracts value for "struct" list from deserialized auth_info
%% json structure
%% @since 2011-08-10 18:19
%%
-spec get_auth_data_list(any()) -> any().

get_auth_data_list({struct, Data}) when is_list(Data) ->
    Data;
get_auth_data_list(_) ->
    [].

%%-----------------------------------------------------------------------------
%%
%% @doc Extracts value for "password" item from deserialized auth_info
%% json structure
%% @since 2011-08-10 18:19
%%
-spec get_auth_password(any()) -> any().

get_auth_password(Data) ->
    get_value(Data, <<"password">>).

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
    end;
get_value(_Data, _Tag) ->
    undefined.

%%-----------------------------------------------------------------------------
