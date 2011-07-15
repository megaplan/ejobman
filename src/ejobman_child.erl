%%%-----------------------------------------------------------------
%%% dynamically added worker
%%%-----------------------------------------------------------------
-module(ejobman_child).
-behaviour(gen_server).
-export([start/0, start_link/0, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).
-include("ejobman.hrl").
-include("amqp_client.hrl").
-define(HTTP_TIMEOUT, 15000).
%-------------------------------------------------------------------
start() ->
    start_link().
%-------------------------------------------------------------------
start_link() ->
    start_link([]).
start_link(Params) ->
    gen_server:start_link(?MODULE, Params, []).
%-------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).
%-------------------------------------------------------------------
init(Params) ->
    C = ejobman_conf:get_config_child(Params),
    p_debug:pr({?MODULE, 'init done', ?LINE, self()},
        C#child.debug, run, 1),
    {ok, C, ?TC}. % yes, this is fast and dirty hack (?TC)
%-------------------------------------------------------------------
handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call(status, _From, St) ->
    {reply, St, St, ?TC};
handle_call(_N, _From, St) ->
    p_debug:pr({?MODULE, 'other', ?LINE, _N, self()},
        St#child.debug, run, 4),
    New = do_smth(St),
    {reply, {error, unknown_request}, New, ?TC}.
%-------------------------------------------------------------------
handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(st0p, St) ->
    St;
handle_cast(_, St) ->
    New = do_smth(St),
    {noreply, New, ?TC}.
%-------------------------------------------------------------------
terminate(_, State) ->
    p_debug:pr({?MODULE, terminate, ?LINE, self()},
        State#child.debug, run, 2),
    ok.
%-------------------------------------------------------------------
handle_info(timeout, State) ->
    p_debug:pr({?MODULE, info_timeout, ?LINE, self()},
        State#child.debug, run, 6),
    New = do_smth(State),
    {noreply, New, ?TC};
handle_info(_Req, State) ->
    p_debug:pr({?MODULE, other, ?LINE, _Req, self()},
        State#child.debug, run, 3),
    New = do_smth(State),
    {noreply, New, ?TC}.
%-------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.
%-------------------------------------------------------------------
% @doc process command, then send stop message to itself
-spec do_smth(#child{}) -> #child{}.

do_smth(State) ->
    process_cmd(State),
    gen_server:cast(self(), stop),
    State#child{method = <<>>, url = <<>>, from = 'undefined'}.
%-------------------------------------------------------------------
% @doc check for a command, do the command, send reply to the client.
-spec process_cmd(#child{}) -> ok.

process_cmd(#child{from = 'undefined'}) ->
    ok;
process_cmd(#child{url = <<>>}) ->
    ok;
process_cmd(#child{method = Method_bin, url = <<_, _/binary>> = Url_bin,
        from = From} = St) ->
    p_debug:pr({?MODULE, 'process_cmd params', ?LINE, self(),
        Method_bin, Url_bin, From}, St#child.debug, run, 3),
    Url = binary_to_list(Url_bin),
    Method = get_method(Method_bin),
    Res = http:request(Method, {Url, []},
        [{timeout, ?HTTP_TIMEOUT}, {connect_timeout, ?HTTP_TIMEOUT}],
        []),
    gen_server:reply(From, Res),
    p_debug:pr({?MODULE, 'process_cmd res', ?LINE, self(), Res},
        St#child.debug, run, 4);
process_cmd(_) ->
    ok.
%-------------------------------------------------------------------
get_method(<<"get">>)  -> get;
get_method(<<"head">>) -> head;
get_method(<<"post">>) -> post;
get_method(_)          -> get.
%-------------------------------------------------------------------
