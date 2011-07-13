-module(ejobman_child_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(RESTARTS, 25).
-define(SECONDS, 5).

start_link() ->
    supervisor:start_link({local, ejobman_child_supervisor},
        ejobman_child_sup,
        []).

init(_Args) ->
    Worker = {
        ejobman_child, {ejobman_child, start_link, []},
        permanent, brutal_kill, worker, [ejobman_child]
        },
    {ok, {{simple_one_for_one, ?RESTARTS, ?SECONDS},
        [Worker]}}.
