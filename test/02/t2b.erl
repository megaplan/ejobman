%%%
%%% mass request to spawn long-lasting workers. For the whole result see
%%% log file
%%%
-module(t2b).
-compile(export_all).

t() ->
    t(20).

t(Max) ->
    F = fun(X) -> spawn(?MODULE, t2, [X]) end,
    lists:map(F, lists:seq(1, Max)).

t2(X) ->
    M = integer_to_list(X),
    U = "http://localhost:8182/?page" ++ integer_to_list(X),
    Res = ejobman_handler:cmd2(M, U, 100),
    error_logger:info_report({?MODULE, t2, res, Res}).
