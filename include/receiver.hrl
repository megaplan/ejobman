-ifndef(receiver).
-define(receiver, true).

-include("nums.hrl").

% state of a receiver gen_server
-record(ejr, {
    pid :: pid(), % own pid
    rses,
    conn,
    log,
    pid_file,

    % for this rt key look into job's group field for real rt key
    temp_rt_key_for_group :: binary(),

    groups = [] :: list(),
    debug = [] :: list()
}).

-endif.
