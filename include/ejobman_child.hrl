-ifndef(ejobman_child).
-define(ejobman_child, true).

-include("nums.hrl").

% state of a worker gen_server
-record(child, {
    name,
    port,
    id,
    os_pid,
    gh_pid,
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

-endif.
