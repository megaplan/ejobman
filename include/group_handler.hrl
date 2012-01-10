-ifndef(group_handler).
-define(group_handler, true).

-include("nums.hrl").

% state of a group_handler gen_server
-record(egh, {
    id         :: reference(), % id that is used by supervisor
    group, % group id
    max        :: non_neg_integer(), % max children for the group
    vhost      :: binary(), % amqp virtual host
    conn,
    exchange   :: binary(), % amqp exchange for a group
    queue      :: binary(), % amqp queue for a group
    debug = [] :: list()
}).

-endif.
