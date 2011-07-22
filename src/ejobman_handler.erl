%%%
%%% ejobman_handler: gen_server that handles messages from ejobman_receiver
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
%%% @doc a gen_server that gets messages from ejobman_receiver and calls
%%% ejobman_child_supervisor to spawn a new child to do all the dirty work
%%%

-module(ejobman_handler).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([cmd/2, remove_child/1]).
-export([cmd2/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").
-include("amqp_client.hrl").

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

%-define(GRP, ejobman_handler_workers).

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(Config) ->
    application:start(inets),
    C = ejobman_conf:get_config_hdl(Config),
    New = prepare_workers(C),
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE}, C#ejm.debug, run, 1),
    {ok, New, ?T}.
%%-----------------------------------------------------------------------------
%%
%% Handling call messages
%% @since 2011-07-15 11:00
%%
-spec handle_call(any(), any(), #ejm{}) ->
    {noreply, #ejm{}, non_neg_integer()}
    | {any(), any(), #ejm{}, non_neg_integer()}.

%% @doc deletes disposable child from the state
handle_call({remove_child, Pid}, _From, St) ->
    St_d = do_smth(St),
    New = remove_child(St_d, Pid),
    {reply, ok, New, ?T};

%% @doc calls disposable child
handle_call({cmd, Method, Url}, From, St) ->
    St_d = do_smth(St),
    New = ejobman_handler_cmd:do_command(St_d, From, Method, Url),
    {noreply, New, ?T};

%% @doc calls long-lasting worker
handle_call({cmd2, Method, Url}, From, St) ->
    St_d = do_smth(St),
    New = ejobman_handler_cmd:do_worker_cmd(St_d, From, Method, Url),
    {noreply, New, ?T};

handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call(status, _From, St) ->
    {reply, St, St, ?T};
handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'other', ?LINE, _N}, St#ejm.debug, run, 3),
    New = do_smth(St),
    {reply, {error, unknown_request}, New, ?T}.
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
    {noreply, New, ?T}.
%%-----------------------------------------------------------------------------
terminate(_, State) ->
    remove_workers(State),
    ok.
%%-----------------------------------------------------------------------------
%%
%% Handling all non call/cast messages
%% @since 2011-07-15 11:00
%%
-spec handle_info(any(), #ejm{}) -> {noreply, #ejm{}, non_neg_integer()}.

handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, info_timeout, ?LINE}, State#ejm.debug, run, 6),
    New = do_smth(State),
    {noreply, New, ?T};
handle_info(_Req, State) ->
    mpln_p_debug:pr({other, ?MODULE, ?LINE, _Req}, State#ejm.debug, run, 3),
    New = do_smth(State),
    {noreply, New, ?T}.
%%-----------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
-spec start() -> any().
%%
%% @doc starts handler gen_server
%% @since 2011-07-15 11:00
%%
start() ->
    start_link().
%%-----------------------------------------------------------------------------
-spec start_link() -> any().
%%
%% @doc starts handler gen_server with pre-defined config
%% @since 2011-07-15 11:00
%%
start_link() ->
    start_link(?CONF).

-spec start_link(string()) -> any().
%%
%% @doc starts handler gen_server with given config
%% @since 2011-07-15 11:00
%%
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).
%%-----------------------------------------------------------------------------
-spec stop() -> any().
%%
%% @doc stops handler gen_server
%% @since 2011-07-15 11:00
%%
stop() ->
    gen_server:call(?MODULE, stop).
%%-----------------------------------------------------------------------------
%%
%% @doc calls any received command to be executed by long-lasting worker
%% @since 2011-07-22 12:00
%%
-spec cmd2(binary(), binary()) -> ok.

cmd2(Method, Url) ->
    gen_server:call(?MODULE, {cmd2, Method, Url}).
%%-----------------------------------------------------------------------------
%%
%% @doc calls any received command to be executed by disposable child
%% @since 2011-07-15 11:00
%%
-spec cmd(binary(), binary()) -> ok.

cmd(Method, Url) ->
    gen_server:call(?MODULE, {cmd, Method, Url}).
%%-----------------------------------------------------------------------------
%%
%% @doc asks ejobman_handler to remove child from the list
%%
-spec remove_child(pid()) -> ok.

remove_child(Pid) ->
    gen_server:call(?MODULE, {remove_child, Pid}).
%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc removes child from the list of children
%%
-spec remove_child(#ejm{}, pid()) -> #ejm{}.

remove_child(#ejm{ch_data=Ch} = St, Pid) ->
    F = fun(#chi{pid=X}) when X == Pid ->
            false;
        (_) ->
            true
    end,
    New = lists:filter(F, Ch),
    St#ejm{ch_data=New}.
%%-----------------------------------------------------------------------------
%%
%% @doc does miscellaneous periodic checks. E.g.: check for children. Returns
%% updated state.
%%
-spec do_smth(#ejm{}) -> #ejm{}.

do_smth(State) ->
    Stw = check_workers(State),
    Stc = check_children(Stw),
    check_queued_commands(Stc).
%%-----------------------------------------------------------------------------
%%
%% @doc calls to process all the queued commands (currently -
%% disposable children only)
%%
-spec check_queued_commands(#ejm{}) -> #ejm{}.

check_queued_commands(St) ->
    ejobman_handler_cmd:do_all_commands(St).
%%-----------------------------------------------------------------------------
%%
%% @doc stops old disengaged workers, stops any too old workers.
%% Returns a new state with quite young workers only
%%
%% @TODO not implemented yet
%%
-spec check_workers(#ejm{}) -> #ejm{}.

check_workers(State) ->
    State
.
%%-----------------------------------------------------------------------------
%%
%% @doc checks that all the children are alive. Returns new state with
%% live children only
%%
-spec check_children(#ejm{}) -> #ejm{}.

check_children(#ejm{ch_data=Ch} = State) ->
    New = lists:filter(fun check_child/1, Ch),
    State#ejm{ch_data = New}.
%%-----------------------------------------------------------------------------
%%
%% @doc checks whether the given child does something
%%
-spec check_child(#chi{}) -> boolean().

check_child(#chi{pid=Pid}) ->
    case process_info(Pid, reductions) of
        {reductions, _N} ->
            true;
        _ ->
            false
    end.
%%-----------------------------------------------------------------------------
-spec prepare_workers(#ejm{}) -> #ejm{}.
%%
%% @doc spawns the configured minimum of long lasting workers
%%
prepare_workers(C) ->
    spawn_workers(C).

-spec spawn_workers(#ejm{}) -> #ejm{}.
%%
%% @doc spawns N workers
%%
spawn_workers(#ejm{min_workers=N} = C) ->
    lists:foldl(
        fun(_X, Acc) -> spawn_one_worker(Acc) end,
        C,
        lists:duplicate(N, true)
    ).
%%-----------------------------------------------------------------------------
-spec spawn_one_worker(#ejm{}) -> #ejm{}.
%%
%% @doc checks for max number of workers. Spawns a new worker if possible.
%%
spawn_one_worker(#ejm{max_workers = Max, workers = Workers} = C) ->
    Len = length(Workers),
    if  Len < Max ->
            real_spawn_one_worker(C);
        true ->
            C
    end.
%%-----------------------------------------------------------------------------
-spec real_spawn_one_worker(#ejm{}) -> #ejm{}.
%%
%% @doc Spawns a new worker, stores its pid (and a ref) in a list,
%% returns the modified state.
%%
real_spawn_one_worker(C) ->
    Id = make_ref(),
    Child_config = make_child_config(C),
    StartFunc = {ejobman_long_worker, start_link, [Child_config]},
    Child = {Id, StartFunc, permanent, 1000, worker, [ejobman_long_worker]},
    Workers = C#ejm.workers,
    Res = supervisor:start_child(ejobman_long_supervisor, Child),
    mpln_p_debug:pr({?MODULE, 'real_spawn_one_worker res', ?LINE, Res},
        C#ejm.debug, run, 3),
    case Res of
        {ok, Pid} ->
            Ch = #chi{pid=Pid, id=Id, start=now()},
            C#ejm{workers = [Ch | Workers]};
        {ok, Pid, _Info} ->
            Ch = #chi{pid=Pid, id=Id, start=now()},
            C#ejm{workers = [Ch | Workers]};
        {error, _Reason} ->
            C
    end.
%%-----------------------------------------------------------------------------
make_child_config(C) ->
    [{debug, C#ejm.debug}]
.
%%-----------------------------------------------------------------------------
-spec remove_workers(#ejm{}) -> ok.
%%
%% @doc terminates all the workers
%%
remove_workers(C) ->
    terminate_workers(C).
%%-----------------------------------------------------------------------------
-spec terminate_workers(#ejm{}) -> ok.
%%
%% @doc terminates all the workers
%%
terminate_workers(#ejm{workers = Workers}) ->
    lists:foreach(fun terminate_one_worker/1, Workers).
%%-----------------------------------------------------------------------------
-spec terminate_one_worker(#chi{}) -> any().
%%
%% @doc terminates one worker
%%
terminate_one_worker(#chi{id=Id}) ->
    supervisor:terminate_child(ejobman_long_supervisor, Id),
    supervisor:delete_child(ejobman_long_supervisor, Id).
%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
remove_child_test() ->
    Pid = self(),
    Me = #chi{pid=Pid, start=now()},
    Ch = make_fake_children(),
    St = #ejm{ch_data = [Me | Ch]},
    ?assert(#ejm{ch_data=Ch} =:= remove_child(St, Pid)).

check_mix_children_test() ->
    Me = #chi{pid=self(), start=now()},
    Ch = make_fake_children(),
    St = #ejm{ch_data = [Me | Ch]},
    ?assert(#ejm{ch_data=[Me]} =:= check_children(St)).
 
check_fake_children_test() ->
    Ch = make_fake_children(),
    St = #ejm{ch_data = Ch},
    ?assert(#ejm{ch_data=[]} =:= check_children(St)).
 
make_fake_children() ->
    L = [
        "<0.12340.5678>",
        "<0.32767.7136>",
        "<0.7575.5433>"
    ],
    lists:map(fun(X) -> #chi{pid=list_to_pid(X), start=now()} end, L).
-endif.
%%-----------------------------------------------------------------------------
