-ifndef(estat).
-define(estat, true).

-include("nums.hrl").

% state of ejobman_stat
-record(est, {
    pid            :: pid(),       % own pid
    start          :: tuple(),     % server start time in now() format
    timer          :: reference(), % timer ref
    timer_log      :: reference(), % timer for log processes memory
    storage = []   :: list(),
    storage_fd     :: pid(),       % current opened storage file
    storage_base   :: string(),    % file name base
    storage_cur_name :: string(),  % expanded (full) name
    storage_start = {0,0,0} :: tuple(), % start time for current storage file
    rotate_interval = 'hour':: never | minute | hour | day
                             | {dow, 0..7} | month | year,
    keep_time      :: non_neg_integer(), % time to keep data in storage. Hours
    flush_interval :: non_neg_integer(), % interval to flush storage. Seconds
    flush_number   :: non_neg_integer(), % number of messages to flush storage.
    flush_last = {0,0,0} :: tuple(),     % last time of flush. Now.
    log_procs_interval   :: non_neg_integer(), % log processes memory
    rt_info_file         :: string(),    % file to write runtime info to
    stat_limit_n   :: non_neg_integer(), % amount
    stat_limit_t   :: non_neg_integer(), % time, seconds
    stat_limit_cnt_h :: non_neg_integer(), % time, hours
    stat_limit_cnt_m :: non_neg_integer(), % time, minutes
    debug
}).

-endif.
