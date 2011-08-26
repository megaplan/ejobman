-ifndef(ejobman_params).
-define(ejobman_params, true).

-define(T, 1000).
-define(TC, 0).
-define(LOG, "/var/log/erpher/ejm").
-define(CONF, "ejobman.conf").

% state of a worker gen_server
-record(child, {
    name,
    port,
    id,
    os_pid,
    duration,
    from,
    method,
    url,
    host,
    auth,
    url_rewrite,
    params,
    debug
}).

-record(chi, {
    pid,
    id,
    mon,
    os_pid,
    start={0,0,0} % time in now() format
}).

-record(pool, {
    id,
    w_duration = 86400, % seconds
    worker_config,
    workers = [] :: [#chi{}],
    waiting = [], % waiting for restart
    restart_delay,
    restart_policy, % restart, none, delay
    w_queue,
    min_workers = 5
}).

% state of a handler and a receiver gen_server
-record(ejm, {
    w_pools = [],
    ch_data, % spawned children
    ch_queue,
    max_children = 32767,
    url_rewrite,
    web_server_pid,
    web_server_opts,
    rses,
    conn,
    log,
    debug
}).

-endif.
