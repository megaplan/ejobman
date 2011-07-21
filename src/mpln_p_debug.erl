%%%
%%% mpln_p_debug: print debug functions
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
%%% @doc print debug functions. These are work if only use_p_debug (see
%%% below) is defined.
%%%

-module(mpln_p_debug).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([p/5, p_ets/6]).
-export([pr/4]).

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

-define(use_p_debug, true).

%%%----------------------------------------------------------------------------
%%% api
%%%----------------------------------------------------------------------------
%%
%% @doc prints string to error log, if configured loglevel higher or equal
%% than hardcoded limit
%% @since 2011-07-15
%%
-spec p(string(), list(), list(), atom(), integer()) -> ok.

-ifdef(use_p_debug).

p(Str, Pars, Conf, Facility, Limit) ->
    Cur_val = proplists:get_value(Facility, Conf, 0),
    if    Cur_val >= Limit ->
            Time = mpln_misc_time:get_time_str_us(),
            error_logger:info_msg(Time ++ "~n" ++ Str, Pars);
        true ->
            ok
    end
.

-else.

p(_Str, _Pars, _Conf, _Facility, _Limit) -> ok.

-endif.

%%
%% @doc prints params to error log, if configured loglevel higher or equal
%% than hardcoded limit
%% @since 2011-07-15
%%
-spec pr(any(), list(), atom(), integer()) -> ok.

-ifdef(use_p_debug).

pr(Param, Conf, Facility, Limit) ->
    Cur_val = proplists:get_value(Facility, Conf, 0),
    if    Cur_val >= Limit ->
            Time = mpln_misc_time:get_time_str_us(),
            error_logger:info_report({Time, Param});
        true ->
            ok
    end
.

-else.

pr(_Param, _Conf, _Facility, _Limit) -> ok.

-endif.

%%
%% @doc prints full ets dump as a list to error log, if configured loglevel
%% higher than coded limit
%% @since 2011-07-15
%%
-spec p_ets(string(), list(), list(), atom(), integer(), atom()) -> ok.

-ifdef(use_p_debug).

p_ets(Str, Pars, Conf, Facility, Limit, Table) ->
    Cur_val = proplists:get_value(Facility, Conf, 0),
    if    Cur_val >= Limit ->
            error_logger:info_msg(Str, Pars),
            List = ets:tab2list(Table),
            error_logger:info_msg("~n~p~n", [List]);
        true ->
            ok
    end
.

-else.

p_ets(_Str, _Pars, _Conf, _Facility, _Limit, _Table) -> ok.

-endif.
%%-----------------------------------------------------------------------------
