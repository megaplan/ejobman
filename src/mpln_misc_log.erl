%%%-----------------------------------------------------------------
%%% functions related to logfiles
%%%-----------------------------------------------------------------

-module(mpln_misc_log).
-export([prepare_log/1]).

%-------------------------------------------------------------------
% @doc creates log file, shuts down tty log in case of log file
% opened successfuly.
-spec prepare_log(string()) -> ok | {error, any()}.

prepare_log(File) ->
    filelib:ensure_dir(File),
    error_logger:logfile(close),
    Fn = get_fname(File),
    case error_logger:logfile({open, Fn}) of
        ok ->
            error_logger:tty(false);
        {error, Reason} ->
            {error, Reason}
    end
.
%-------------------------------------------------------------------
get_fname(File) ->
    T = mpln_misc_time:get_ts(),
    lists:flatten([File ++ "_" ++ T])
.
%-------------------------------------------------------------------
