-ifndef(ejobman_job).
-define(ejobman_job, true).

%-define(T, 1000).

-record(job, {
    id :: reference() | binary(),
    type = 'rest',
    method,
    url,
    host,
    ip,
    auth,
    params,
    group,
    tag, % delivery tag for amqp
    path :: string(),
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
    result  :: {ok, any()} | {error, any()}, % short result of http request
    result_full :: tuple(), % full result of http request
    dur_all :: non_neg_integer(), % the whole duration
    dur_req :: non_neg_integer(), % duration of http request
    start,         % start time (fetch from internal queue)
    t_start_child, % start child time
    t_stop_child,  % stop child time
    t_start_req,   % start http request time
    t_stop_req,    % stop http request time
    time           % last change time
}).

-record(rt, {
    type = 'now' :: 'now' | 'at' | 'in',
    at,
    in
}).

-endif.
