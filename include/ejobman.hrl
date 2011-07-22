-ifndef(ejobman_params).
-define(ejobman_params, true).

-define(T, 1000).
-define(TC, 0).
-define(LOG, "/var/log/ejobman/ejm").
-define(CONF, "ejobman.conf").

-record(child, {
    from,
    method,
    url,
    debug
}).

-record(chi, {
    pid,
    id,
    start={0,0,0} % time in now() format
}).

-record(ejm, {
    workers,
    w_queue,
    min_workers = 5,
    max_workers = 255,
    ch_data, % spawned children
    ch_queue,
    max_children = 32767,
    rses,
    conn,
    log,
    debug
}).

-endif.
