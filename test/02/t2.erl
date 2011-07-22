%%%
%%% mass request to spawn disposable children. The whole result is in log.
%%%
-module(t2).
-compile(export_all).

t() ->
    t(20).

t(Max) ->
    F = fun(X) -> spawn(?MODULE, t2, [X]) end,
    lists:map(F, lists:seq(1, Max)).

t2(X) ->
    M = integer_to_list(X),
    U = "http://localhost:8182/?page" ++ integer_to_list(X),
    Res = gen_server:call(ejobman_handler, {cmd, M, U}),
    error_logger:info_report({?MODULE, t2, res, Res}).
