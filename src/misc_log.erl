%%%-----------------------------------------------------------------
%%% functions related to logfiles
%%%-----------------------------------------------------------------

-module(misc_log).
-export([prepare_log/1]).

%-------------------------------------------------------------------
-spec prepare_log(string()) -> ok | {error, any()}.

prepare_log(File) ->
	filelib:ensure_dir(File),
	error_logger:logfile(close),
	Fn = get_fname(File),
	case error_logger:logfile({open, Fn}) of
		ok ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end
.
%-------------------------------------------------------------------
get_fname(File) ->
	T = misc_time:get_ts(),
	lists:flatten([File ++ "_" ++ T])
.
%-------------------------------------------------------------------
