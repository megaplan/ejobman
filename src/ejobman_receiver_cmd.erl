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
%%% @doc functions that do real handling of the payload received via AMQP
%%%

-module(ejobman_receiver_cmd).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([store_rabbit_cmd/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("ejobman.hrl").
-include("rabbit_session.hrl").

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

-define(HTTP_TIMEOUT, 15000).

%%%----------------------------------------------------------------------------
%%% api
%%%----------------------------------------------------------------------------

%%
%% @doc sends received command to a command handler. Returns nothing actually.
%% @since 2011-07-15
%%
-spec store_rabbit_cmd(#ejm{}, binary()) -> #ejm{}.

store_rabbit_cmd(State, Bin) ->
    p_debug:pr({?MODULE, 'store_rabbit_cmd json', ?LINE, Bin},
        State#ejm.debug, run, 4),
    case catch mochijson2:decode(Bin) of
        {'EXIT', Reason} ->
            p_debug:pr({?MODULE, 'store_rabbit_cmd error', ?LINE, Reason},
                State#ejm.debug, run, 2);
        Data ->
            Type = mpln_misc_json:get_type(Data),
            proceed_cmd_type(State, Type, Data)
    end,
    State.
%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc calls ejobman_handler with received command info
%%
-spec proceed_cmd_type(#ejm{}, binary(), any()) -> ok.

proceed_cmd_type(State, <<"rest">>, Data) ->
    Info = mpln_misc_json:get_job_info(Data),
    Method = mpln_misc_json:get_method(Info),
    Url = mpln_misc_json:get_url(Info),
    % timeout on child crash leads to exception
    Res = (catch ejobman_handler:cmd(Method, Url)),
    p_debug:pr({?MODULE, 'proceed_cmd_type res', ?LINE, Res},
        State#ejm.debug, run, 5);
proceed_cmd_type(State, Other, _Data) ->
    p_debug:pr({?MODULE, 'proceed_cmd_type other', ?LINE, Other},
        State#ejm.debug, run, 2).
%%-----------------------------------------------------------------------------
