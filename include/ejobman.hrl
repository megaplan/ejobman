-ifndef(ejobman_params).
-define(ejobman_params, true).

-define(T, 1000).
%-define(LOG, "/var/log/nums/n").
-define(CONF, "ejobman.conf").

-record(ejm, {
	rses,
	conn,
	rabbit,
	log,
	debug
}).

-endif.
