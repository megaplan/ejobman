-ifndef(estat).
-define(estat, true).

-include("nums.hrl").

% state of ejobman_stat
-record(est, {
    tid,
    timer          :: reference(), % timer ref
    storage = []   :: list(),
    storage_fd     :: pid(),       % current opened storage file
    storage_base   :: string(),    % file name base
    storage_cur_name :: string(),  % expanded (full) name
    storage_start = {0,0,0} :: tuple(), % start time for current storage file
    rotate_interval = 'hour':: never | minute | hour | day
                             | {dow, 0..7} | month | year,
    keep_time      :: non_neg_integer(), % time to keep data in storage. Minutes
    flush_interval :: non_neg_integer(), % interval to flush storage. Seconds
    flush_number   :: non_neg_integer(), % number of messages to flush storage.
    flush_last = {0,0,0} :: tuple(),     % last time of flush. Now.
    debug
}).

-endif.
