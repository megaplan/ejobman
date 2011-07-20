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
    start={0,0,0} % time in now() format
}).

-record(ejm, {
    ch_data, % spawned children
    max_children = 32767,
    rses,
    conn,
    log,
    debug
}).

-endif.
