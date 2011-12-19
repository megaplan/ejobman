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

% job status used in statistic of last N jobs
-record(jst, {
    job     :: #job{},
    status  :: 'queued' | 'sent' | 'done',
    result  :: {ok, any()} | {error, any()}, % result of http request
    dur_all :: non_neg_integer(), % the whole duration
    dur_req :: non_neg_integer(), % duration of http request
    start, % start time
    time   % last change time
}).

-record(rt, {
    type = 'now' :: 'now' | 'at' | 'in',
    at,
    in
}).

-endif.
