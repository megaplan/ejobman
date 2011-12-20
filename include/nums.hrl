-ifndef(ejobman_nums).
-define(ejobman_nums, true).

-define(STAT_T, 1000).
-define(STAT_KEEP_TIME, 4320). % minutes
-define(STAT_FLUSH_INTERVAL, 300). % seconds
-define(STAT_STORAGE, "/var/lib/erpher/estat.dat").
-define(STAT_LIMIT_N, 100). % amount
-define(STAT_LIMIT_T, 100). % seconds

-define(HTTP_CONNECT_TIMEOUT, 15000).
-define(HTTP_TIMEOUT, 3600000).
-define(T, 1000).
-define(TC, 0).
-define(LOG, "/var/log/erpher/ejm").
-define(CONF, "ejobman.conf").

-endif.
