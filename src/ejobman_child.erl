%%%
%%% ejobman_child: dynamically added worker
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
%%% @doc dynamically added worker that does the real thing.
%%%

-module(ejobman_child).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------
-export([start/0, start_link/0, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").
-include("job.hrl").
-include("amqp_client.hrl").
-include("sign_req.hrl").

%-define(HTTP_TIMEOUT, 15000).
-define(CTYPE, "application/x-www-form-urlencoded").

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(Params) ->
    C = ejobman_conf:get_config_child(Params),
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE, C#child.id, self()},
        C#child.debug, run, 2),
    {ok, C, ?TC}. % yes, this is fast and dirty hack (?TC)

%%-----------------------------------------------------------------------------
%%
%% Handling call messages
%% @since 2011-07-15 11:00
%%
-spec handle_call(any(), any(), #ejm{}) -> {stop|reply, any(), any(), any()}.

handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call(status, _From, St) ->
    {reply, St, St, ?TC};
handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'other', ?LINE, _N, St#child.id, self()},
        St#child.debug, run, 2),
    New = do_smth(St),
    {reply, {error, unknown_request}, New, ?TC}.

%%-----------------------------------------------------------------------------
%%
%% Handling cast messages
%% @since 2011-07-15 11:00
%%
-spec handle_cast(any(), #ejm{}) -> any().

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(st0p, St) ->
    St;
handle_cast(_, St) ->
    New = do_smth(St),
    {noreply, New, ?TC}.

%%-----------------------------------------------------------------------------
terminate(_, State) ->
    ejobman_handler:remove_child(self(), State#child.group),
    mpln_p_debug:pr({?MODULE, terminate, ?LINE, State#child.id, self()},
        State#child.debug, run, 2),
    ok.

%%-----------------------------------------------------------------------------
%%
%% Handling all non call/cast messages
%%
-spec handle_info(any(), #child{}) -> any().

handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, info_timeout, ?LINE, State#child.id, self()},
        State#child.debug, run, 6),
    New = do_smth(State),
    {noreply, New, ?TC};
handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, other, ?LINE, _Req, State#child.id, self()},
        State#child.debug, run, 2),
    New = do_smth(State),
    {noreply, New, ?TC}.

%%-----------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% api
%%%----------------------------------------------------------------------------
start() ->
    start_link().
%%-----------------------------------------------------------------------------
start_link() ->
    start_link([]).
start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).
%%-----------------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc processes command, then sends stop message to itself
%% @since 2011-07-15
%%
-spec do_smth(#child{}) -> #child{}.

do_smth(State) ->
    process_cmd(State),
    gen_server:cast(self(), stop),
    State#child{method = <<>>, url = <<>>,
        params = 'undefined', from = 'undefined'}.

%%-----------------------------------------------------------------------------
%%
%% @doc checks the command, then follows real_cmd
%% @since 2011-07-15
%%
-spec process_cmd(#child{}) -> ok.

process_cmd(#child{from = 'undefined'}) ->
    ok;
process_cmd(#child{url = <<>>}) ->
    ok;
process_cmd(#child{url = [_ | _]} = St) ->
    real_cmd(St);
process_cmd(#child{url = <<_, _/binary>> = Url_bin} = St) ->
    Url = binary_to_list(Url_bin),
    real_cmd(St#child{url = Url});
process_cmd(_) ->
    ok.

%%-----------------------------------------------------------------------------
%%
%% @doc does the command, sends reply to the client.
%% NOTE: in case of a big number of requests the TCP_WAIT problem can
%% arise. To handle that use other clients:
%% https://bitbucket.org/etc/lhttpc/wiki/Home
%% https://github.com/cmullaparthi/ibrowse
%% @since 2011-07-18
%%
real_cmd(#child{id=Id, method=Method_bin, params=Params,
        http_connect_timeout=Conn_t, http_timeout=Http_t} = St) ->
    mpln_p_debug:pr({?MODULE, "real_cmd params", ?LINE, St#child.id, self(),
        St}, St#child.debug, run, 4),
    Method = ejobman_clean:get_method(Method_bin),
    Method_str = ejobman_clean:get_method_str(Method),
    {Url, Hdr} = make_url(St, Method_str),
    Req = make_req(Method, Url, Hdr, Params),
    mpln_p_debug:pr({?MODULE, "real_cmd request", ?LINE, St#child.id, self(),
        Req}, St#child.debug, http, 4),
    Res = http:request(Method, Req,
        [{timeout, Http_t}, {connect_timeout, Conn_t}],
        []),
    % reply should be either removed or changed
    % in according with ejobman_handler:cmd
    %gen_server:reply(From, Res),
    ejobman_handler:cmd_result(Res, Id),
    mpln_p_debug:log_http_res({?MODULE, ?LINE}, Res, St#child.debug).

%%-----------------------------------------------------------------------------
%%
%% @doc converts url to string, rewrites url according to the config
%% @since 2011-08-08 13:53
%%
-spec make_url(#child{}, string()) -> {string(), list()}.

make_url(#child{url=Bin} = St, Method) ->
    Url = ejobman_clean:get_url(Bin),
    case http_uri:parse(Url) of
        %{https,"l:p","host.localdomain",123,"/goo","?foo=baz"}
        %{Scheme, Auth, Host, Port, Path, Query}
        {error, _Reason} ->
            H = compose_headers(St, [], "", "", "", ""),
            {Url, H};
        Data ->
            rewrite(St, Method, Url, Data)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc rewrites host part of an url according to the config, adds
%% host header if necessary
%% @since 2011-08-08 13:53
%%
-spec rewrite(#child{}, string(), string(), tuple()) -> {string(), list()}.

rewrite(#child{url_rewrite=Rew_conf} = St, Method, Url,
        {_Scheme, _Auth, Host, _Port, Path, Query} = Data) ->
    case find_matching_host(Rew_conf, Host) of
        {ok, Config} ->
            rewrite_host(St, Config, Method, Url, Data);
        error ->
            H = compose_headers(St, [], Method, Host, Path, Query),
            {Url, H}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a list containing host and auth headers
%%
-spec compose_headers(#child{}, list(), string(), any(), string(), string()) ->
    list().

compose_headers(St, Config, Method, Url_host, Path, Query) ->
    A = compose_auth_header(St#child.auth, Method, Url_host, Path, Query),
    H = compose_host_header(Config, St#child.host, Url_host),
    lists:flatten([A, H]).

%%-----------------------------------------------------------------------------
%%
%% @doc finds config section that matches the host. Returns this section
%% or error if no section found
%%
-spec find_matching_host(list(), string()) -> {ok, list()} | error.

find_matching_host(Conf, Host) ->
    F = fun(X) ->
        not match_one_host(X, Host)
    end,
    case lists:dropwhile(F, Conf) of
        [H | _] ->
            {ok, H};
        _ ->
            error
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc matches the host against one section of url_rewrite config
%%
-spec match_one_host(list(), string()) -> boolean().

match_one_host(List, Host) ->
    Src_url = proplists:get_value(src_host_part, List),
    case proplists:get_value(src_type, List) of
        regex ->
            match_one_host_regex(Host, Src_url);
        _ ->
            Host == Src_url
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc matches one host with regex from url_rewrite part.
%% @todo move re:compile to ejobman_handler config preparation
%%
match_one_host_regex(_Host, 'undefined') ->
    false;
match_one_host_regex(Host, Src_url) ->
    case re:compile(Src_url) of
        {ok, Mp} ->
            re:run(Host, Mp, [{capture, none}]) == match;
        _ ->
            false
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc rewrites host in the input url according to the configured
%% in url_rewrite parameters. Returns either new url or old url
%% (in case of no dst_host_part defined in Pars)
%%
-spec rewrite_host(#child{}, list(), string(), string(), tuple()) ->
    {string(), list()}.

rewrite_host(St, Config, Method, Url,
        {Scheme, Auth, Host, Port, Path, Query}) ->
    mpln_p_debug:pr({?MODULE, 'rewrite_host pars', ?LINE, self(),
        Config, Host, Url}, St#child.debug, rewrite, 4),
    case proplists:get_value(dst_host_part, Config) of
        undefined ->
            New_url = Url;
        New_host ->
            New_url = proceed_rewrite_host(St, Scheme, Auth, New_host,
                Port, Path, Query)
    end,
    H = compose_headers(St, Config, Method, Host, Path, Query),
    {New_url, H}.

%%-----------------------------------------------------------------------------
%%
%% @doc returns either a list with an "Authorization" header tuple or
%% an empty list.
%%
-spec compose_auth_header(#auth{}, string(), string(), string(), string()) ->
    list().

compose_auth_header(#auth{type=basic} = Auth, _, _, _, _) ->
    Str = make_hdr_auth_str(Auth),
    make_auth_header(Str);
compose_auth_header(#auth{type=megaplan, auth_key=undefined}, _, _, _, _) ->
    [];
compose_auth_header(#auth{type=megaplan, secret_key=undefined}, _, _, _, _) ->
    [];
compose_auth_header(#auth{type=megaplan, auth_key=Akey, secret_key=Skey},
        Method, Host, Path, Query) ->
    Req = #req{
        method = Method,
        content_md5 = "", % FIXME: fin^W temporary solution
        ctype = ?CTYPE,
        host = Host,
        uri = make_auth_req_uri(Path, Query)
    },
    A_str = mpln_misc_web:make_string(Akey),
    S_str = mpln_misc_web:make_string(Skey),
    {Sign, Time} = ejobman_req_sign:make_sign(Req, A_str, S_str),
    [
        {"type", "megaplan"},
        {"X-Authorization", Sign},
        {"Date", Time}
    ];
compose_auth_header(_, _, _, _, _) ->
    [].

%%-----------------------------------------------------------------------------
%%
%% @doc joins path and query using "?"
%%
make_auth_req_uri(Path, "") ->
    Path;
make_auth_req_uri(Path, Query) ->
    Path ++ "?" ++ Query.

%%-----------------------------------------------------------------------------
%%
%% @doc returns either a list with a "Host" header tuple or an empty list.
%% "Host" header is filled in the following order: host from config,
%% host from request, host from source url
%%
-spec compose_host_header(list(), string(), string()) -> list().

compose_host_header(Config, Req_host, Url_host) ->
    case proplists:get_value(dst_host_hdr, Config) of
        undefined ->
            New_host = select_host_field(Req_host, Url_host);
        New_host ->
            ok
    end,
    make_host_header(New_host).

%%-----------------------------------------------------------------------------
select_host_field(<<>>, Url_host) ->
    Url_host;
select_host_field([], Url_host) ->
    Url_host;
select_host_field('undefined', Url_host) ->
    Url_host;
select_host_field(Req_host, _Url_host) ->
    Req_host.

%%-----------------------------------------------------------------------------
%%
%% @doc creates an url string based on parsed parts of the original url
%%
-spec proceed_rewrite_host(#child{}, atom(), list(), list(), integer(),
    list(), list()) -> string().

proceed_rewrite_host(St, Scheme, Auth, Host, Port, Path, Query) ->
    % http_uri:parse("http://l:p@host.localdomain/goo?foo=baz")
    % {https,"l:p","host.localdomain",123,"/goo","?foo=baz"}
    if  is_atom(Scheme) ->
            Scheme_str = [atom_to_list(Scheme), "://"];
        true ->
            Scheme_str = ""
    end,
    Auth_str = make_url_auth_str(Auth),
    Port_str = integer_to_list(Port),
    Str = [Scheme_str, Auth_str, Host, ":", Port_str, Path, Query],
    Res_str = lists:flatten(Str),
    mpln_p_debug:pr({?MODULE, 'proceed_rewrite_host res', ?LINE, self(),
        Scheme, Auth, Host, Port, Path, Query, Res_str},
        St#child.debug, rewrite, 5),
    Res_str.

%%-----------------------------------------------------------------------------
%%
%% @doc gets #auth record and creates the string ready for use in
%% the Authorization header
%%
-spec make_hdr_auth_str(#auth{}) -> string().

make_hdr_auth_str(#auth{user=undefined}) ->
    [];
make_hdr_auth_str(Auth) ->
    List = make_auth_str([], Auth),
    Enc = base64:encode_to_string(lists:flatten(List)),
    lists:flatten(["Basic ", Enc])
.

%%-----------------------------------------------------------------------------
%%
%% @doc gets the original auth string from url and creates a list of strings
%% ready for use in url
%%
make_url_auth_str("") ->
    "";
make_url_auth_str(Str) ->
    [make_auth_str(Str, []), "@"].

%%-----------------------------------------------------------------------------
%%
%% @doc gets the original auth string from url and auth data from request.
%% returns list of strings that ready for use in either url or further header
%% creating
%%
make_auth_str([], #auth{type=basic, user=User, password=Pass}) ->
    U = mpln_misc_web:make_string(User),
    P = mpln_misc_web:make_string(Pass),
    [U, ":", P];
make_auth_str(<<>>, #auth{type=basic, user=User, password=Pass}) ->
    U = mpln_misc_web:make_string(User),
    P = mpln_misc_web:make_string(Pass),
    [U, ":", P];
make_auth_str('undefined', #auth{type=basic, user=User, password=Pass}) ->
    U = mpln_misc_web:make_string(User),
    P = mpln_misc_web:make_string(Pass),
    [U, ":", P];
make_auth_str(Auth_url, _Req) ->
    [Auth_url].

%%-----------------------------------------------------------------------------
%%
%% @doc creates a http request
%% @since 2011-08-04 17:49
%%
make_req(head, Url, Hdr, _Params) ->
    {Url, Hdr};
make_req(get, Url, Hdr, _Params) ->
    {Url, Hdr};
make_req(post, Url, Hdr, Params) ->
    Ctype = ?CTYPE,
    Body = make_body(Params),
    {Url, Hdr, Ctype, Body}.

%%-----------------------------------------------------------------------------
%%
%% @doc makes either an empty list or a list with a tuple containing the
%% auth header ready for use in http client
%%
-spec make_auth_header(any()) -> [] | [{string(), string()}].

make_auth_header([]) ->
    [];
make_auth_header(H) ->
    [{"Authorization", H}].

%%-----------------------------------------------------------------------------
%%
%% @doc makes either an empty list or a list with a tuple containing the
%% Host header ready to use in http client
%%
-spec make_host_header(any()) -> [] | [{string(), string()}].

make_host_header([]) ->
    [];
make_host_header("undefined") ->
    [];
make_host_header('undefined') ->
    [];
make_host_header(H) when is_binary(H) ->
    [{"Host", binary_to_list(H)}];
make_host_header(H) when is_atom(H) ->
    [{"Host", atom_to_list(H)}];
make_host_header(H) when is_tuple(H) ->
    Str = inet_parse:ntoa(H),
    [{"Host", Str}];
make_host_header(H) ->
    [{"Host", H}].

%%-----------------------------------------------------------------------------
%%
%% @doc creates the body (kind of...) of a http request
%% @since 2011-08-04 17:49
%%
make_body(Pars) ->
    mpln_misc_web:query_string(Pars).
    %mochiweb_util:urlencode(Pars).

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
process_cmd_test() ->
    ok = process_cmd(#child{}),
    ok = process_cmd(#child{from = 'undefined'}),
    ok = process_cmd(#child{url = <<>>}),
    ok = process_cmd(#child{url = ""}),
    ok = process_cmd([]).

make_url_rewrite_conf() ->
    [
        [
            {src_host_part, "192.168.9.183"},
            {dst_host_hdr, "promo.megaplan.kulikov"}
        ],
        [
            {src_host_part, "promo.megaplan.kulikov"},
            {dst_host_part, "192.168.9.183"},
            {dst_host_hdr, "promo.megaplan.kulikov"}
        ],
        [
            {src_type, regex},
            {src_host_part, "127\\.0\\.0\\.\\d+"},
            {dst_host_part, "127.0.0.1"},
            {dst_host_hdr, "host1.localdomain"}
        ]
    ].

match_one_host_test() ->
    Conf = make_url_rewrite_conf(),
    Item = lists:last(Conf),
    Host = "127.0.0.2",
    Res = match_one_host(Item, Host),
    ?assert(Res)
.

find_matching_host_regex_test() ->
    Conf = make_url_rewrite_conf(),
    Host = "127.0.0.2",
    {ok, Item} = find_matching_host(Conf, Host),
    Item_orig = [
        {src_type, regex},
        {src_host_part, "127\\.0\\.0\\.\\d+"},
        {dst_host_part, "127.0.0.1"},
        {dst_host_hdr, "host1.localdomain"}
    ],
    ?assert(Item =:= Item_orig)
.

find_matching_host_test() ->
    Conf = make_url_rewrite_conf(),
    Host = "192.168.9.183",
    {ok, Item} = find_matching_host(Conf, Host),
    Item_orig = [
        {src_host_part, "192.168.9.183"},
        {dst_host_hdr, "promo.megaplan.kulikov"}
    ],
    ?assert(Item =:= Item_orig)
.

make_url1_test() ->
    Conf = make_url_rewrite_conf(),
    Bin = <<"http://192.168.9.183/new/order/send-messages">>,
    R1 = make_url(#child{url=Bin, url_rewrite=Conf, debug=[]}, post),
    R0 = { "http://192.168.9.183/new/order/send-messages", 
        [{"Host", "promo.megaplan.kulikov"}]},
    ?assert(R0 =:= R1),
    ok
.

make_url2_test() ->
    Conf = make_url_rewrite_conf(),
    Bin = <<"http://192.168.9.183/new/order/send-messages">>,
    R1 = make_url(#child{url=Bin, url_rewrite=Conf, debug=[],
        auth=#auth{user="usr1", password="psw2"}}, post),
    R0 = { "http://192.168.9.183/new/order/send-messages", 
        [{"Authorization","Basic dXNyMTpwc3cy"},
            {"Host", "promo.megaplan.kulikov"}]},
    %mpln_p_debug:pr({?MODULE, ?LINE, R0, R1}, [], run, 0),
    ?assert(R0 =:= R1),
    ok
.

rewrite_host_test() ->
    Url = "192.168.9.183",
    Url2 = "promo.megaplan.kulikov",
    Pars = [
        {src_host_part, Url},
        {dst_host_part, Url2}
    ],
    Link = "http://" ++ Url ++ ":80/new/order/send-messages",
    Data = http_uri:parse(Link),
    Data2 = rewrite_host(#child{debug=[]}, Pars, post, Url, Data),
    Data3 = {"http://" ++ Url2 ++ ":80/new/order/send-messages",
        [{"Host", "192.168.9.183"}]},
    ?assert(Data2 =:= Data3)
.

-endif.
%%-----------------------------------------------------------------------------
