%%%-----------------------------------------------------------------
%%% ejobman handler
%%%-----------------------------------------------------------------
-module(ejobman_handler).
-behaviour(gen_server).
-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([cmd/2]).

-include("ejobman.hrl").
-include("amqp_client.hrl").
%-------------------------------------------------------------------
start() ->
    start_link().
%-------------------------------------------------------------------
start_link() ->
    start_link(?CONF).
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).
%-------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).
%-------------------------------------------------------------------
init(Config) ->
    application:start(inets),
    C = ejobman_conf:get_config_hdl(Config),
    p_debug:pr({?MODULE, 'init done', ?LINE}, C#ejm.debug, run, 1),
    {ok, C, ?T}.
%-------------------------------------------------------------------
handle_call({cmd, Method, Url}, From, St) ->
    New = ejobman_handler_cmd:do_command(St, From, Method, Url),
    {noreply, New, ?T};
handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call(status, _From, St) ->
    {reply, St, St, ?T};
handle_call(_N, _From, St) ->
    p_debug:pr({?MODULE, 'other', ?LINE, _N}, St#ejm.debug, run, 4),
    {reply, {error, unknown_request}, St, ?T}.
%-------------------------------------------------------------------
handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(st0p, St) ->
    St;
handle_cast(_, St) ->
    {noreply, St, ?T}.
%-------------------------------------------------------------------
terminate(_, _State) ->
    ok.
%-------------------------------------------------------------------
handle_info(timeout, State) ->
    p_debug:pr({?MODULE, info_timeout, ?LINE}, State#ejm.debug, run, 6),
    {noreply, State, ?T};
handle_info(_Req, State) ->
    p_debug:pr({other, ?MODULE, ?LINE, _Req}, State#ejm.debug, run, 3),
    {noreply, State, ?T}.
%-------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.
%-------------------------------------------------------------------
% @doc api to call any command.
-spec cmd(binary(), binary()) -> ok.

cmd(Method, Url) ->
    gen_server:call(?MODULE, {cmd, Method, Url})
.
%-------------------------------------------------------------------
