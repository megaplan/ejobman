-ifndef(ejobman_job).
-define(ejobman_job, true).

%-define(T, 1000).

-record(job, {
    id :: reference(),
    type = 'rest',
    method,
    url,
    host,
    auth,
    params,
    group,
    tag, % delivery tag for amqp
    run_time
}).

-record(auth, {
    type = 'basic' :: 'basic' | 'megaplan',
    user,
    password,
    auth_key, % for megaplan auth
    secret_key, % for megaplan auth
    data % headers ([{key, val}]) supplied by user
}).

-record(rt, {
    type = 'now' :: 'now' | 'at' | 'in',
    at,
    in
}).

-endif.
