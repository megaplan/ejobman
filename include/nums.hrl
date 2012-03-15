-ifndef(ejobman_nums).
-define(ejobman_nums, true).

-define(LOG_PROCS_INTERVAL, 60). % seconds
-define(GID_DEFAULT, default).

-define(STAT_T_KEEP_MINUTES, 62).
-define(STAT_T_KEEP_HOURS, 26).
-define(STAT_T, 1000).
-define(STAT_KEEP_TIME, 72). % hours
-define(STAT_CLEAN_INTERVAL, 60). % seconds
-define(STAT_FLUSH_INTERVAL, 60). % seconds
-define(STAT_FLUSH_NUMBER, 100). % amount
-define(STAT_STORAGE, "/var/lib/erpher/estat").
-define(STAT_LIMIT_N, 100). % amount
-define(STAT_LIMIT_T, 100). % seconds
-define(STAT_LIMIT_CT_H, 25). % hours
-define(STAT_LIMIT_CT_M, 61). % minutes

-define(HTTP_CONNECT_TIMEOUT, 15000).
-define(HTTP_TIMEOUT, 3600000).
-define(T, 1000).
-define(TC, 0).
-define(LOG, "/var/log/erpher/ejm").
-define(CONF, "ejobman.conf").

-define(TABC, "border=1 cellspacing=4 cellpadding=4 frame=border rules=all").

-define(STAT_TAB_M, ejobman_stat_t_m).
-define(STAT_TAB_H, ejobman_stat_t_h).

-endif.
