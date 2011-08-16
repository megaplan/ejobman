%%%
%%% mpln_misc_time: time processing functions
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
%%% @doc functions related to time processing
%%%

-module(mpln_misc_time).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([get_ts/0]).
-export([uuid/0]).
-export([get_time_str_us/0, get_time_str_us/1]).
-export([get_time_str/0, get_time_str/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%----------------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------------
%% copied from stdlib/calendar.erl
%%
-type year()     :: non_neg_integer().
%-type year1970() :: 1970..10000.    % should probably be 1970..
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.
%-type daynum()   :: 1..7.
%-type ldom()     :: 28 | 29 | 30 | 31. % last day of month

-type t_now()    :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}.

-type t_date()         :: {year(),month(),day()}.
-type t_time()         :: {hour(),minute(),second()}.
-type t_datetime()     :: {t_date(),t_time()}.
%-type t_datetime1970() :: {{year1970(),month(),day()},t_time()}.

-type second_f()       :: float().
-type t_time_f()       :: {hour(),minute(),second_f()}.
-type t_datetime_f()   :: {t_date(),t_time_f()}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc returns time string in gregorian seconds for current time
%% @since 2011-07-15
%%
-spec get_ts() -> string().

get_ts() ->
    Str = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    integer_to_list(Str).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s.us) for current time
%% @since 2011-07-15
%%
-spec get_time_str_us() -> string().

get_time_str_us() ->
    get_time_str_us(now()).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s.us) for defined time
%% @since 2011-07-15
%%
-spec get_time_str_us(t_now()) -> string().

get_time_str_us({_, _, Us} = Now) ->
    {Date, {H, M, S}} = calendar:now_to_local_time(Now),
    make_str_float({Date, {H, M, S + Us/1000000.0}}).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s) for current time
%% @since 2011-08-16 19:05
%%
-spec get_time_str() -> string().

get_time_str() ->
    get_time_str(now()).

%%-----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s) for defined time
%% @since 2011-08-16 19:05
%%
-spec get_time_str(t_now()) -> string().

get_time_str(Now) ->
    DT = calendar:now_to_local_time(Now),
    make_str_int(DT).

%%-----------------------------------------------------------------------------
%%
%% @doc returns binary filled by current time to be used as uuid
%% @since 2011-07-15
%%
-spec uuid() -> binary().

uuid() ->
    {A, B, C} = now(),
    <<A:32, B:32, C:32>>.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc returns time string (y-m-d h:m:s.us) for given time
%% @since 2011-07-15
%%
-spec make_str_float(t_datetime_f()) -> string().

make_str_float(DateTime) ->
    make_str("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~9.6.0f", DateTime).

%%
%% @doc returns time string (y-m-d h:m:s) for given time
%% @since 2011-07-15
%%
-spec make_str_int(t_datetime()) -> string().

make_str_int(DateTime) ->
    make_str("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", DateTime).

%%
%% @doc returns time string (y-m-d h:m:s) according to the given format and
%% datetime
%% @since 2011-07-15
%%
-spec make_str(string(), t_datetime() | t_datetime_f()) -> string().

make_str(Format, {{Year, Mon, Day}, {Hour, Min, Sec}}) ->
    Str = io_lib:format(Format, [Year, Mon, Day, Hour, Min, Sec]),
    lists:flatten(Str).

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
make_str_test() ->
    "0111-01-07 12:05:*********" = make_str(
        "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~9.6.0f",
        {{111, 1, 7}, {12, 5, 503.0045}}),
    "0111-01-07 12:05:03.004500" = make_str(
        "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~9.6.0f",
        {{111, 1, 7}, {12, 5, 3.0045}}).

make_str_float_test() ->
    "0111-01-07 12:05:03.004500" =
        make_str_float({{111, 1, 7}, {12, 5, 3.0045}}),
    "8111-01-07 12:05:*********" =
        make_str_float({{8111, 1, 7}, {12, 5, 503.0045}}).

get_time_str_us_test() ->
    ?_assertException(error, function_clause,
        get_time_str_us({{8111, 1, 7}, {12, 5, 503.0045}})
        )
.
-endif.
