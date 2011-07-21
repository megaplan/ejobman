%%%
%%% mpln_misc_conf: config file read and processing
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
%%% @doc functions related to config file read, config processing
%%%

-module(mpln_misc_conf).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([read_config/1]).

%%%----------------------------------------------------------------------------
%%% api
%%%----------------------------------------------------------------------------
%%
%% @doc reads the given config file using file:consult/1, returns
%% the config as a list. In case of an error returns an empty list.
%% @since 2011-07-15
%%
-spec read_config(string()) -> list().

read_config(File) ->
    case file:consult(File) of
        {ok, [H|_]} ->
            H;
        {error, Reason} ->
            error_logger:info_msg("~p:read_config:~p error:~n~p~n",
                [?MODULE, ?LINE, Reason]),
            []
    end.
%%-----------------------------------------------------------------------------
