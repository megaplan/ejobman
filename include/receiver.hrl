-ifndef(receiver).
-define(receiver, true).

-include("nums.hrl").

% state of a receiver gen_server
-record(ejr, {
    rses,
    conn,
    log,
    pid_file,
    debug
}).

-endif.
