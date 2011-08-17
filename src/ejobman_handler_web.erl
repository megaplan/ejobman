%%%
%%% ejobman_handler_web: start web server
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
%%% @since 2011-08-17 13:40
%%% @license MIT
%%% @doc start web server that serves the monitoring page
%%%

-module(ejobman_handler_web).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([prepare_web/1, dispatch/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc prepare web server which is used to serve handler monitoring page only
%% @since 2011-08-17 13:40
%%
prepare_web(C) ->
    case C#ejm.web_server_opts of
        [_|_] ->
            List = proplists:delete(loop, C#ejm.web_server_opts),
            Opts = [{loop, {?MODULE, dispatch, [C]}} | List],
            {ok, Https} = mochiweb_http:start(Opts),
            C#ejm{web_server_pid = Https};
        _ ->
            C
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc dispatcher for web requests
%% @since 2011-08-17 13:40
%%
dispatch(Req, C) ->
    mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Req}, C#ejm.debug, http, 5),
    case Req:get(method) of
        'GET' ->
            get_resource(C, Req);
        _ ->
            Headers = [{"Allow", "GET"}],
            Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc gets status2 from ejobman_handler and sends it as plain text response
%% to the client of the web server
%%
get_resource(C, Req) ->
    Path = Req:get(path),
    case Path of
        "/status2" ->
            Res = ejobman_handler:get_status2(),
            Str = io_lib:format("~p", [Res]),
            mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Res},
                C#ejm.debug, http, 4),
            Req:ok({"text/plain", Str});
        _ ->
            Req:respond({404, [], "404 Not Found\r\n"})
    end.

%%-----------------------------------------------------------------------------
