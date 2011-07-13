%%%-----------------------------------------------------------------
%%% functions related to time processing
%%%-----------------------------------------------------------------
-module(misc_time).
-export([get_ts/0]).
-export([uuid/0]).
-export([get_time_str_us/0, get_time_str_us/1]).
%-------------------------------------------------------------------
get_ts() ->
    Str = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    integer_to_list(Str)
.
%-------------------------------------------------------------------
get_time_str_us() ->
    get_time_str_us(now())
.
%-------------------------------------------------------------------
get_time_str_us({_, _, Us} = Now) ->
    {Date, {H, M, S}} = calendar:now_to_local_time(Now),
    make_str_float({Date, {H, M, S + Us/1000000.0}})
.
%-------------------------------------------------------------------
make_str_float(DateTime) ->
    make_str("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~9.6.0f", DateTime)
.
%-------------------------------------------------------------------
make_str_int(DateTime) ->
    make_str("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", DateTime)
.
%-------------------------------------------------------------------
make_str(Format, {{Year, Mon, Day}, {Hour, Min, Sec}}) ->
    Str = io_lib:format(Format, [Year, Mon, Day, Hour, Min, Sec]),
    lists:flatten(Str)
.
%-------------------------------------------------------------------
uuid() ->
    {A, B, C} = now(),
    <<A:32, B:32, C:32>>
.
%-------------------------------------------------------------------
