-ifndef(ejobman_params).
-define(ejobman_params, true).

-define(T, 1000).
-define(LOG, "/var/log/ejobman/ejm").
-define(CONF, "ejobman.conf").

-record(ejm, {
    rses,
    conn,
    log,
    debug
}).

-endif.
