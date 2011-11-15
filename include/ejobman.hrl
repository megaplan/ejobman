-ifndef(ejobman_params).
-define(ejobman_params, true).

-define(HTTP_CONNECT_TIMEOUT, 15000).
-define(HTTP_TIMEOUT, 3600000).
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
    group,
    duration,
    from,
    method,
    url,
    host,
    auth,
    url_rewrite,
    http_connect_timeout,
    http_timeout,
    params,
    debug
}).

-record(jgroup, {
    id,
    max_children
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
    ch_queues, % dict: group -> queue of jobs
    ch_data, % dict: group -> spawned children list
%    default_queue, % default queue
%    default_ch_data = [], % default spawned children list
    max_children = 32767,
    http_connect_timeout = ?HTTP_CONNECT_TIMEOUT,
    http_timeout = ?HTTP_TIMEOUT,
    url_rewrite,
    web_server_pid,
    web_server_opts,
    rses,
    conn,
    log,
    pid_file,
    job_groups = [], % configured job groups
    job_log, % filename for job log
    job_log_last,
    job_log_rotate :: minute | hour | day | month,
    jlog, % file descriptor
    jlog_f, % expanded file name
    debug
}).

-endif.
