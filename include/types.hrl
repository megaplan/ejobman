-ifndef(mpln_types).
-define(mpln_types, true).

%% from stdlib/calendar.erl

-type year()     :: non_neg_integer().
%-type year1970() :: 1970..10000.    % should probably be 1970..
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.
%-type daynum()   :: 1..7.
%-type ldom()     :: 28 | 29 | 30 | 31. % last day of month

-type t_now()    :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}.

-type t_date()         :: {year(),month(),day()}.
-type t_time()         :: {hour(),minute(),second()}.
-type t_datetime()     :: {t_date(),t_time()}.
%-type t_datetime1970() :: {{year1970(),month(),day()},t_time()}.

-type second_f()       :: float().
-type t_time_f()       :: {hour(),minute(),second_f()}.
-type t_datetime_f()   :: {t_date(),t_time_f()}.

-endif.
