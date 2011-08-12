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
    type = 'basic' :: 'basic' | 'megaplan',
    user,
    password,
    data % headers ([{key, val}]) supplied by user
}).

-record(rt, {
    type = 'now' :: 'now' | 'at' | 'in',
    at,
    in
}).

-endif.
