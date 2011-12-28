%%%
%%% ejobman_print_stat: create statistic output
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
%%% @since 2011-12-28 13:12
%%% @license MIT
%%% @doc functions that create output of statistic data
%%%

-module(ejobman_print_stat).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([make_stat_queue_info/1]).
-export([make_stat_t_info/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").
-include("nums.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc returns time statistic
%% @since 2011-12-28 12:30
%%
-spec make_stat_t_info(#ejm{}, text | html) -> string() | list().

make_stat_t_info(St, raw) ->
    get_stat_t_info(St);

make_stat_t_info(St, text) ->
    make_stat_t_info_text(St);

make_stat_t_info(St, html) ->
    make_stat_t_info_html(St).

%%-----------------------------------------------------------------------------
%%
%% @doc returns state of queues: name, length
%% @since 2011-12-27 18:09
%%
-spec make_stat_queue_info(#ejm{}) -> string().

make_stat_queue_info(St) ->
    List = get_stat_queue_info(St),
    F = fun({K, V}) ->
                io_lib:format("~p: ~p~n", [K, V])
        end,
    lists:flatten(lists:map(F, List)).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc creates time statistic as html
%%
make_stat_t_info_html(St) ->
    List = get_stat_t_info(St),
    F = fun({{Dt, Group}, {Cur, Max}}) ->
                [
                 "<tr>",
                 "<td>", mpln_misc_time:make_str2_int(Dt), "</td>",
                 "<td>", mpln_misc_web:make_string(Group), "</td>",
                 "<td>", mpln_misc_web:make_string(Cur), "</td>",
                 "<td>", mpln_misc_web:make_string(Max), "</td>",
                 "</tr>\n"
                ];
           ({K, V}) ->
                [
                 "<tr>",
                 "<td colspan=2>",
                 mpln_misc_web:make_term_string(K),
                 "</td>",
                 "<td colspan=2>",
                 mpln_misc_web:make_term_string(V),
                 "</td>",
                 "</tr>\n"
                ]
        end,
    F_big = fun({Tag, L}) ->
                    [
                     "<html><body>\n<p>\n",
                     mpln_misc_web:make_term_string(Tag),
                     "<p>\n",
                     "<table ", ?TABC, ">",
                     "<tr><td></td>",
                     "<td>time</td>",
                     "<td>group</td>",
                     "<td>current</td>",
                     "<td>max</td></tr>\n",
                     lists:map(F, L),
                     "<table>\n",
                     "<p>\n",
                     "</body></html>\n"
                    ]
            end,
    lists:flatten(lists:map(F_big, List)).

%%-----------------------------------------------------------------------------
%%
%% @doc creates time statistic as text
%%
make_stat_t_info_text(St) ->
    List = get_stat_t_info(St),
    F = fun({K, V}) ->
                io_lib:format("~p: ~p~n", [K, V])
        end,
    F_big = fun({Tag, L}) ->
                    [io_lib:format("~p~n~n", [Tag]),
                     lists:map(F, L),
                     io_lib:format("----------------------------------------"
                                   "~n~n", [])
                    ]
            end,
    lists:flatten(lists:map(F_big, List)).

%%-----------------------------------------------------------------------------
%%
%% @doc fetches queue names and sizes
%%
-spec get_stat_t_info(#ejm{}) -> [{any(), any()}].

get_stat_t_info(#ejm{stat_t=Stat}) ->
    Min = get_stat_t_info2(Stat#stat_t.m),
    Hour = get_stat_t_info2(Stat#stat_t.h),
    [{"minute data", Min}, {"hour data", Hour}].

get_stat_t_info2(Data) ->
    lists:sort(dict:to_list(Data)).

%%-----------------------------------------------------------------------------
%%
%% @doc fetches queue names and sizes
%%
-spec get_stat_queue_info(#ejm{}) -> [{any(), non_neg_integer()}].

get_stat_queue_info(#ejm{ch_queues=Data}) ->
    F = fun(Gid, Cur, Acc) ->
                Len = queue:len(Cur),
                [{Gid, Len} | Acc]
        end,
    dict:fold(F, [], Data).

%%-----------------------------------------------------------------------------
