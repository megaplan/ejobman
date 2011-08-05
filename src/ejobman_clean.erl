%%%
%%% ejobman_clean: miscellaneous clean up functions
%%%

-module(ejobman_clean).

-export([get_method/1, get_url/1]).

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
%% @doc transforms input data to the atom containing one of methods:
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
