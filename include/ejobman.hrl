-ifndef(ejobman_params).
-define(ejobman_params, true).

-include("nums.hrl").

% state of a worker gen_server
-record(child, {
    name,
    port,
    id,
    os_pid,
    group,
    tag,
    duration,
    from,
    method,
    url,
    host,
    ip,
    auth,
    schema_rewrite = [],
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
    tag,
    alive=true,
    stop={0,0,0}, % time of marking dead
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

-record(stat_t, {
          m :: dict(), % stat with 1 minute step. {time, group} -> {cur, max}
          h :: dict()  % stat with 1 hour step. {time, group} -> {cur, max}
         }).

% state of a handler and a receiver gen_server
-record(ejm, {
    ch_queues :: dict(), % dict: group -> queue of jobs
    ch_data   :: dict(), % dict: group -> spawned children list
    max_children = 32767,
    http_connect_timeout = ?HTTP_CONNECT_TIMEOUT,
    http_timeout = ?HTTP_TIMEOUT,
    schema_rewrite,
    url_rewrite,
    web_server_pid,
    web_server_opts,
    log,
    pid_file,
    group_handler = [], % config for group handlers
    group_handler_run = [], % started group handlers. Unnecessary, in fact
    job_groups = [], % configured job groups
    job_log, % filename for job log
    job_log_last,
    job_log_rotate :: never | minute | hour | day | {dow, 0..7} | month | year,
    jlog, % file descriptor
    jlog_f, % expanded file name
    stat_t       :: #stat_t{}, % time series
    stat_r       :: dict(), % last N job information
    stat_limit_n :: non_neg_integer(), % amount
    stat_limit_t :: non_neg_integer(), % time, seconds
    debug
}).

-endif.
