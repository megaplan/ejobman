%%%
%%% ejobman_clean: miscellaneous clean up functions
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
%%% @doc miscellaneous clean up functions
%%% 

-module(ejobman_clean).

-export([get_method/1, get_url/1]).
-export([get_method_str/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc transforms input data to the atom containing one of the methods:
%% get/post/head
%% @since 2011-07-25 15:00
%%
get_method(A) when is_atom(A) ->
    Str = string:to_lower(atom_to_list(A)),
    get_method(Str);
get_method(B) when is_binary(B) ->
    Str = string:to_lower(binary_to_list(B)),
    get_method(Str);
get_method(L) when is_list(L) ->
    get_method_aux(string:to_lower(L));
get_method(D) ->
    get_method_aux(D).
    
get_method_aux("get")      -> get;
get_method_aux("head")     -> head;
get_method_aux("post")     -> post;
get_method_aux(_)          -> head.

%%-----------------------------------------------------------------------------
%%
%% @doc transforms input atom to upper case string
%% @since 2011-09-14 16:14
%%
get_method_str(A) ->
    string:to_upper(atom_to_list(A)).

%%-----------------------------------------------------------------------------
%%
%% @doc transforms input data to a string
%% @since 2011-08-05 19:22
%%
get_url(A) when is_atom(A) ->
    atom_to_list(A);
get_url(B) when is_binary(B) ->
    binary_to_list(B);
get_url(L) ->
    L.

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
get_method_test() ->
    List = [
        {get, ["get", 'get', <<"GeT">>, "Get", 'gET']},
        {post, ["post", "PoST", <<"pOsT">>, 'pOSt']},
        {head, ['heAD', "HEad", <<"hEaD">>, "head"]},
        {head, ['options', "OPTions", <<"OpTiOnS">>, "TraCE", <<"any_other">>]}
    ],
    get_method_test_list(List).

get_method_test_list(List) ->
    lists:map(fun get_method_test_one_row/1, List).

get_method_test_one_row({Key, Vals}) ->
    lists:map(fun(X) -> get_method_test_one_item(Key, X) end, Vals).

get_method_test_one_item(Key, Item) ->
    Key = get_method(Item).
-endif.
%%-----------------------------------------------------------------------------
