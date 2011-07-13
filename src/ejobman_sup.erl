-module(ejobman_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(RESTARTS, 5).
-define(SECONDS, 2).

start_link() ->
    supervisor:start_link({local, ejobman_supervisor}, ejobman_sup, []).

init(_Args) ->
    Receiver = {
        ejobman_receiver, {ejobman_receiver, start_link, []},
        permanent, brutal_kill, worker, [ejobman_receiver]
        },
    Handler = {
        ejobman_handler, {ejobman_handler, start_link, []},
        permanent, brutal_kill, worker, [ejobman_handler]
        },
    Sup = {
        ejobman_child_sup, {ejobman_child_sup, start_link, []},
        transient, infinity, supervisor, [ejobman_child_sup]
        },
    {ok, {{one_for_one, ?RESTARTS, ?SECONDS},
        [Receiver, Handler, Sup]}}.
