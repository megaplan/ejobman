-ifndef(ejobman_job).
-define(ejobman_job, true).

%-define(T, 1000).

-record(job, {
    type = 'rest',
    method,
    url,
    host,
    params,
    run_time
}).

-record(rt, {
    type = 'now' :: 'now' | 'at' | 'in',
    at,
    in
}).

-endif.
