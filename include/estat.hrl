-ifndef(estat).
-define(estat, true).

-include("nums.hrl").

% state of ejobman_stat
-record(est, {
    timer          :: reference(), % timer ref
    storage        :: string(),    % file name
    tid,                           % storage id
    keep_time      :: non_neg_integer(), % time to keep data in storage. Minutes
    flush_interval :: non_neg_integer(), % interval to flush storage. Seconds
    debug
}).

-endif.
