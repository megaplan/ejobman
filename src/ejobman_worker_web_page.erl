%%%
%%% ejobman_worker_web_page: create web page
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
%%% @since 2011-08-19 15:35
%%% @license MIT
%%% @doc create web page
%%%

-module(ejobman_worker_web_page).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([create_plain_status/1, create_html_status/1]).

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

-define(TABC, "border=1 cellspacing=4 cellpadding=4 frame=border rules=all").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc gets a server status and creates a text
%%
create_plain_status(S) ->
    Str = io_lib:format("~p", [S]),
    {"text/plain", Str}.

%%-----------------------------------------------------------------------------
%%
%% @doc gets a server status and creates a nice looking html. Or at least
%% the html that doesn't make your eyes bleed
%%
-spec create_html_status(list()) -> {string(), string()}.

create_html_status(Pools) ->
    Str = format_page(Pools),
    %Str = io_lib:format("~p", [S]),
    {"text/html", Str}.

%%-----------------------------------------------------------------------------
%%
%% @doc creates html page with pool information
%%
format_page(Pools) ->
    List = lists:map(fun format_one_pool/1, Pools),
    Page = "<html><body>\n" ++
        List ++
        "</body></html>\n",
    lists:flatten(Page).

%%-----------------------------------------------------------------------------
%%
%% @doc creates a html table with one pool information
%%
format_one_pool(Pool) ->
    Data = create_one_pool_data(Pool),
    Text_list = lists:map(fun format_pool_line/1, Data),
    "<p><table " ++ ?TABC ++ ">" ++ Text_list ++ "</table>\n</p>\n".

%%-----------------------------------------------------------------------------
%%
%% @doc creates a list with items to be used in html table
%%
create_one_pool_data(Pool) ->
    Workers_data = proplists:get_value(workers, Pool),
    Waiting_data = proplists:get_value(waiting, Pool),
    [
        {pool_id, proplists:get_value(pool_id, Pool)},
        {work_duration, proplists:get_value(work_duration, Pool)},
        {worker_config, proplists:get_value(worker_config, Pool)},
        {workers, format_workers(Workers_data), text},
        {waiting, format_waiting(Waiting_data), text},
        {restart_policy, proplists:get_value(restart_policy, Pool)},
        {restart_delay, proplists:get_value(restart_delay, Pool)},
        {queue_len, proplists:get_value(queue_len, Pool)},
        {min_workers, proplists:get_value(min_workers, Pool)},
        {max_workers, proplists:get_value(max_workers, Pool)}
    ].

%%-----------------------------------------------------------------------------
%%
%% @doc creates one html table row based on pool data
%%
format_pool_line({Key, Val}) ->
    format_pool_line({Key, Val, none});
format_pool_line({Key, Val, none}) ->
    Style = get_item_style([Key]),
    Line = io_lib:format("<td ~s>~p</td>~n<td ~s>~p</td>",
        [Style, Key, Style, Val]),
    "<tr>" ++ Line ++ "</tr>";
format_pool_line({Key, Val, text}) ->
    Style = get_item_style([Key]),
    Line = io_lib:format("<td ~s>~p</td>~n<td ~s>~s</td>",
        [Style, Key, Style, Val]),
    "<tr>" ++ Line ++ "</tr>".

%%-----------------------------------------------------------------------------
%%
%% @doc creates html item with waiting queue data
%% @todo make it real
%%
format_waiting(List) ->
    Text_list = lists:map(fun format_one_line/1, List),
    Flat_text = lists:flatten(Text_list),
    "<table " ++ ?TABC ++ ">" ++ Flat_text ++ "</table>".

%%-----------------------------------------------------------------------------
%%
%% @doc creates a small html table with worker information in every row
%%
-spec format_workers(list()) -> string().

format_workers(List) ->
    Head = {erlang_pid, os_pid, start_time},
    Text_list = lists:map(fun format_one_line/1, [Head | List]),
    Flat_text = lists:flatten(Text_list),
    "<table " ++ ?TABC ++ ">" ++ Flat_text ++ "</table>".

%%-----------------------------------------------------------------------------
%%
%% @doc returns table row
%%
-spec format_one_line(tuple()) -> string().

format_one_line(Data) ->
    format_one_line(Data, "").

%%-----------------------------------------------------------------------------
%%
%% @doc returns table row finished by terminator
%%
-spec format_one_line(tuple(), string()) -> string().

format_one_line(Data, Term) ->
    List = tuple_to_list(Data),
    Style = get_item_style(List),
    F = fun(X) ->
        format_one_item(X, Style)
    end,
    Line_data = lists:map(F, List),
    "<tr>" ++ Line_data ++ "</tr>" ++ Term.

%%-----------------------------------------------------------------------------
%%
%% @doc returns one td cell
%%
format_one_item(Item, Style) ->
    io_lib:format("<td ~s>~p</td>", [Style, Item]).

%%-----------------------------------------------------------------------------
%%
%% @doc adds color to the header of tables
%%
get_item_style([erlang_pid | _]) ->
    "bgcolor=\"#CCCCDA\"";
get_item_style([pool_id | _]) ->
    "bgcolor=\"#88CC88\"";
get_item_style(_) ->
    "".

%%-----------------------------------------------------------------------------
