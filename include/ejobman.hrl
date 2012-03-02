-ifndef(ejobman_params).
-define(ejobman_params, true).

-include("nums.hrl").
-include("chi.hrl").

-record(jgroup, {
    id,
    max_children
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
    pid       :: pid(),  % own pid
    ch_data   :: dict(), % dict: group -> spawned children list
    max_children = 32767,
    web_server_pid,
    web_server_opts,
    log,
    pid_file,
    group_handler = [], % config for group handlers
    group_handler_run = [], % started group handlers
    job_groups = [], % configured job groups
    timer        :: reference(),       % timer for periodic actions
    debug
}).

-endif.
