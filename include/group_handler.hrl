-ifndef(group_handler).
-define(group_handler, true).

-include("nums.hrl").

% state of a group_handler gen_server
-record(egh, {
    id         :: reference(), % id that is used by supervisor
    group, % group id
    max        :: non_neg_integer(), % max children for the group
    ch_run = []:: list(),   % running children
    ch_queue   :: queue(),  % fetched from amqp but not yet started children
    vhost      :: binary(), % amqp virtual host
    conn,
    exchange   :: binary(), % amqp exchange for a group
    queue      :: binary(), % amqp queue for a group
    http_connect_timeout = ?HTTP_CONNECT_TIMEOUT :: non_neg_integer(),
    http_timeout         = ?HTTP_TIMEOUT         :: non_neg_integer(),
    schema_rewrite = []  :: list(),
    url_rewrite = []     :: list(),
    jit_log_data, % ets id
    jit_log_level        :: non_neg_integer(),
    jit_log_keep_n       :: non_neg_integer(),
    jit_log_keep_time    :: non_neg_integer(),
    debug = []           :: list()
}).

-endif.
