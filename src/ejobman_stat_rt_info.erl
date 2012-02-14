%%%
%%% ejobman_stat_rt_info: write runtime stats
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
%%% @since 2012-02-14 15:45
%%% @license MIT
%%% @doc writes runtime info to external file to be used by external tools
%%% for monitoring purposes
%%% 

-module(ejobman_stat_rt_info).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([write_rt_info/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

-include("estat.hrl").

%%-----------------------------------------------------------------------------
%%
%% @doc writes (or don't if the file undefined) runtime info to the file
%% @since 2012-02-14 15:45
%%
-spec write_rt_info(#est{}, tuple()) -> ok.

write_rt_info(#est{rt_info_file=undefined}, _Res) ->
    ok;

write_rt_info(#est{rt_info_file=File} = St, Dat) ->
    List = erlang:tuple_to_list(Dat),
    Str = make_string(List),
    Temp = File ++ ".tmp",
    case file:write_file(Temp, Str) of
        ok ->
            rename_files(St, File, Temp);
        Other ->
            mpln_p_debug:pr({?MODULE, 'write_rt_info error', ?LINE, Other},
                St#est.debug, stat, 0)
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc renames temp file to the real one
%%
rename_files(St, File, Temp) ->
    case file:rename(Temp, File) of
        ok ->
            ok;
        Other ->
            mpln_p_debug:pr({?MODULE, 'rename_files error', ?LINE, Other},
                St#est.debug, stat, 0)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc makes list of strings, joins them with delimiter space, adds new line
%% in the end
%%
-spec make_string(list()) -> list().

make_string(List) ->
    F = fun(X) ->
        io_lib:format("~p", [X])
    end,
    L2 = lists:map(F, List),
    [string:join(L2, " "), "\n"].

%%-----------------------------------------------------------------------------
