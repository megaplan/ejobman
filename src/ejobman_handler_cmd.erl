%%%
%%% ejobman_handler_cmd: received command handling
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
%%% @doc functions that do real handling of the command received by
%%% ejobman_handler
%%%

-module(ejobman_handler_cmd).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([do_command/4]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").

%%%----------------------------------------------------------------------------
%%% api
%%%----------------------------------------------------------------------------
%%
%% @doc checks for reaching the configured maximum number of children.
%% Proceeds to command if there is a space for a new child.
%%
-spec do_command(#ejm{}, any(), binary(), binary()) -> #ejm{}.

do_command(#ejm{ch_data=Ch, max_children = Max} = St, From, Method, Url) ->
    Len = length(Ch),
    if  Len < Max ->
            proceed_command(St, From, Method, Url);
        true ->
            mpln_p_debug:pr({?MODULE, 'do_command too many children', ?LINE,
                Len, Max}, St#ejm.debug, run, 2),
            St
    end.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
%%
%% @doc do command processing in background then send reply to the client
%%
-spec proceed_command(#ejm{}, any(), binary(), binary()) -> #ejm{}.

proceed_command(St, From, Method, Url) ->
    mpln_p_debug:pr({?MODULE, 'proceed_command cmd', ?LINE, Method, Url},
        St#ejm.debug, run, 4),
    % parameters for ejobman_child
    Params = [
        {from, From},
        {method, Method},
        {url, Url},
        {debug, St#ejm.debug}
        ],
    Res = supervisor:start_child(ejobman_child_supervisor, [Params]),
    mpln_p_debug:pr({?MODULE, 'proceed_command res', ?LINE, Res},
        St#ejm.debug, run, 4),
    case Res of
        {ok, Pid} ->
            add_child(St, Pid);
        _ ->
            St
    end.
%%-----------------------------------------------------------------------------
%% @doc adds child's pid to the list for later use
%% (e.g.: assign a job, kill, rip, etc...)
-spec add_child(#ejm{}, pid()) -> #ejm{}.

add_child(#ejm{ch_data=Children} = St, Pid) ->
    Ch = #chi{pid = Pid, start = now()},
    St#ejm{ch_data = [Ch | Children]}
.
%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
do_command_test() ->
    Pid = self(),
    Me = #chi{pid=Pid, start=now()},
    St = #ejm{ch_data=[Me], max_children = 1, debug=[{run, -1}]},
    New = do_command(St, {self(), 'tag'}, "head", "http://localhost:8182/"),
    ?assert(St =:= New).
-endif.
%%-----------------------------------------------------------------------------
