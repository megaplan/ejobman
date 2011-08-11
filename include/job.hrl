-ifndef(ejobman_job).
-define(ejobman_job, true).

%-define(T, 1000).

-record(job, {
    type = 'rest',
    method,
    url,
    host,
    auth,
    params,
    run_time
}).

-record(auth, {
    type = 'basic',
    user,
    password
}).

-record(rt, {
    type = 'now' :: 'now' | 'at' | 'in',
    at,
    in
}).

-endif.
