%%%-----------------------------------------------------------------
%%% command receiver. Gets AMQP messages with commands.
%%%-----------------------------------------------------------------
-module(ejobman_receiver).
-behaviour(gen_server).
-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).
-include("ejobman.hrl").
-include("amqp_client.hrl").
%-------------------------------------------------------------------
start() ->
    start_link().
%-------------------------------------------------------------------
start_link() ->
    start_link(?CONF).
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).
%-------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).
%-------------------------------------------------------------------
init([Config]) ->
    application:start(inets),
    C = ejobman_conf:get_config(Config),
    New = prepare_all(C),
    p_debug:pr({'init done', ?MODULE, ?LINE}, New#ejm.debug, run, 1),
    {ok, New, ?T}.
%-------------------------------------------------------------------
handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call(status, _From, St) ->
    {reply, St, St, ?T};
handle_call(_N, _From, St) ->
    p_debug:p("~p::~p other:~n~p~n",
        [?MODULE, ?LINE, _N], St#ejm.debug, run, 4),
    {reply, {error, unknown_request}, St, ?T}.
%-------------------------------------------------------------------
handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(rotate, St) ->
    misc_log:prepare_log(St#ejm.log),
    {noreply, St, ?T};
handle_cast(st0p, St) ->
    St;
handle_cast(_, St) ->
    {noreply, St, ?T}.
%-------------------------------------------------------------------
terminate(_, #ejm{conn=Conn} = _State) ->
    ejobman_rb:teardown(Conn),
    ok.
%-------------------------------------------------------------------
handle_info(timeout, State) ->
    p_debug:pr({?MODULE, 'info_timeout', ?LINE}, State#ejm.debug, run, 6),
    {noreply, State, ?T};
handle_info({#'basic.deliver'{delivery_tag = _Tag}, Content} = _Req, State) ->
    p_debug:p("~p::~p basic.deliver:~n~p~n",
        [?MODULE, ?LINE, _Req], State#ejm.debug, run, 5),
    Payload = Content#amqp_msg.payload,
    ejobman_rb:send_ack(State#ejm.conn, _Tag),
    New = ejobman_receiver_cmd:store_rabbit_cmd(State, Payload),
    {noreply, New, ?T};
handle_info(_Req, State) ->
    p_debug:pr({?MODULE, 'other', ?LINE, _Req}, State#ejm.debug, run, 3),
    {noreply, State, ?T}.
%-------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.
%-------------------------------------------------------------------
% @doc does all necessary preparations: [re]opens log file.
-spec prepare_all(#ejm{}) -> #ejm{}.

prepare_all(C) ->
    misc_log:prepare_log(C#ejm.log),
    prepare_q(C).
%-------------------------------------------------------------------
prepare_q(C) ->
    {ok, Conn} = ejobman_rb:start(C#ejm.rses),
    C#ejm{conn=Conn}.
%-------------------------------------------------------------------
