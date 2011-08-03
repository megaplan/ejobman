%%%
%%% test for add_pool api
%%%
-module(t4).
-compile(export_all).

t() ->
    List = [
        {id, test_pool_1},
        {worker, [
            {name, "/usr/bin/perl"},
            {duration, 600000},
            {debug,
                [
                    {run, 3},
                    {ftp, 3}
                ]
            }
        ]},
        {worker_duration, 120000},
        {min_workers, 3},
        {max_workers, 23}
    ],
    t(List).

t(List) ->
    ok = ejobman_handler:add_pool(List),
    S1 = gen_server:call(ejobman_handler, status),
    S1
.
