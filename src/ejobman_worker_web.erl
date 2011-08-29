%%%
%%% ejobman_worker_web: start web server
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

-module(ejobman_worker_web).

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
%% @doc prepare web server which is used to serve worker monitoring page only
%% @since 2011-08-17 13:40
%%
-spec prepare_web(#ejm{}) -> #ejm{}.

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
-spec dispatch(any(), #ejm{}) -> any().

dispatch(Req, C) ->
    mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Req}, C#ejm.debug, http, 5),
    case Req:get(method) of
        'GET' ->
            Path = Req:get(path),
            Type = get_query_type(Req),
            mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Path, Type},
                C#ejm.debug, http, 5),
            get_resource(C, Req, Path, Type);
        _ ->
            Headers = [{"Allow", "GET"}],
            Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc gets status2 from ejobman_worker and sends it as plain text response
%% to the client of the web server
%%
-spec get_resource(#ejm{}, any(), any(), any()) -> any().

get_resource(C, Req, "/status2", "full") ->
    Res = ejobman_worker:get_status2(),
    mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Res}, C#ejm.debug, http, 4),
    Response = ejobman_worker_web_page:create_plain_status(Res),
    Req:ok(Response);
get_resource(C, Req, "/status2", _Type) ->
    Res = ejobman_worker:get_status2(),
    mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Res}, C#ejm.debug, http, 4),
    Response = ejobman_worker_web_page:create_html_status(Res),
    Req:ok(Response);
get_resource(_C, Req, _Path, _Type) ->
    Req:respond({404, [], "404 Not Found\r\n"}).

%%-----------------------------------------------------------------------------
%%
%% @doc extracts query type from the request
%%
get_query_type(Req) ->
    Q = Req:parse_qs(),
    proplists:get_value("type", Q).

%%-----------------------------------------------------------------------------
