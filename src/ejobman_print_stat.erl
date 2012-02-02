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

-export([make_stat_cur_info/1]).
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
-spec make_stat_cur_info(#ejm{}) -> string().

make_stat_cur_info(St) ->
    Winfo = make_stat_work_info(St),
    Qinfo = make_stat_queue_info(St),
    List = [{"working", Winfo}, {"queued", Qinfo}],
    F = fun({Tag, L}) ->
                    [io_lib:format("~p~n~n", [Tag]),
                     L,
                     io_lib:format("----------------------------------------"
                                   "~n~n", [])
                    ]
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
    F = fun({{Dt, Group}, {W_cur, W_max, Q_cur, Q_max}}) ->
                [
                 "<tr>",
                 "<td>", mpln_misc_time:make_str2_int(Dt), "</td>",
                 "<td>", mpln_misc_web:make_string(Group), "</td>",
                 "<td>", mpln_misc_web:make_string(W_cur), "</td>",
                 "<td>", mpln_misc_web:make_string(W_max), "</td>",
                 "<td>", mpln_misc_web:make_string(Q_cur), "</td>",
                 "<td>", mpln_misc_web:make_string(Q_max), "</td>",
                 "</tr>\n"
                ];
           ({K, V}) ->
                [
                 "<tr>",
                 "<td colspan=2>",
                 mpln_misc_web:make_term_string(K),
                 "</td>",
                 "<td colspan=4>",
                 mpln_misc_web:make_term_string(V),
                 "</td>",
                 "</tr>\n"
                ]
        end,
    F_big = fun({Tag, L}) ->
                    [
                     "<html><body>\n<p>\n",
                     "<p>\n",
                     "<table ", ?TABC, ">",
                     "<tr><td colspan=6 bgcolor=\"#CCCCDA\">",
                     mpln_misc_web:make_term_string(Tag),
                     "</td></tr>\n",
                     "<tr>",
                     "<td>time</td>",
                     "<td>group</td>",
                     "<td>work current</td>",
                     "<td>work max</td>\n",
                     "<td>queue current</td>",
                     "<td>queue max</td>",
                     "</tr>\n",
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

get_stat_t_info(St) ->
    Min = get_stat_t_info2(?STAT_TAB_M),
    Hour = get_stat_t_info2(?STAT_TAB_H),
    [{"minute data", Min}, {"hour data", Hour}].

get_stat_t_info2(Tab) ->
    lists:sort(ets:tab2list(Tab)).

%%-----------------------------------------------------------------------------
%%
%% @doc fetches names and sizes from a working stat dictionary
%%
-spec get_stat_work_info(dict()) -> [{any(), non_neg_integer()}].

get_stat_work_info(Data) ->
    F = fun(Gid, Cur, Acc) ->
                Len = length(Cur),
                [{Gid, Len} | Acc]
        end,
    dict:fold(F, [], Data).

%%-----------------------------------------------------------------------------
%%
%% @doc creates a {queued_group, length} list
%%
-spec make_stat_queue_info(#ejm{}) -> list().

make_stat_queue_info(_St) ->
    make_list([]).

%%-----------------------------------------------------------------------------
%%
%% @doc creates a {working_group, length} list
%%
-spec make_stat_work_info(#ejm{}) -> list().

make_stat_work_info(St) ->
    List = get_stat_work_info(St#ejm.ch_data),
    make_list(List).

%%-----------------------------------------------------------------------------
%%
%% @doc makes a text representation of a {key, value} list
%%
-spec make_list(list()) -> list().

make_list(List) ->
    F = fun({K, V}) ->
                io_lib:format("~p: ~p~n", [K, V])
        end,
    lists:map(F, List).

%%-----------------------------------------------------------------------------
