%%%
%%% mpln_misc_log: logfile related functions
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
%%% @doc logfile related functions
%%%

-module(mpln_misc_log).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([prepare_log/1]).

%%%----------------------------------------------------------------------------
%%% api
%%%----------------------------------------------------------------------------
%%
%% @doc creates log file, shuts down tty log in case of log file
%% opened successfuly.
%%
-spec prepare_log(string()) -> ok | {error, any()}.

prepare_log(File) ->
    filelib:ensure_dir(File),
    error_logger:logfile(close),
    Fn = get_fname(File),
    case error_logger:logfile({open, Fn}) of
        ok ->
            error_logger:tty(false);
        {error, Reason} ->
            {error, Reason}
    end.
%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
get_fname(File) ->
    T = mpln_misc_time:get_ts(),
    lists:flatten([File ++ "_" ++ T]).
%%-----------------------------------------------------------------------------
