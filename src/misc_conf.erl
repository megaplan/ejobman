%%%-----------------------------------------------------------------
%%% functions related to config file read, config processing
%%%-----------------------------------------------------------------
-module(misc_conf).
-export([read_config/1]).
%-------------------------------------------------------------------
-spec read_config(string()) -> list().

read_config(File) ->
    case file:consult(File) of
        {ok, [H|_]} ->
            H;
        {error, Reason} ->
            error_logger:info_msg("~p:read_config:~p error:~n~p~n",
                [?MODULE, ?LINE, Reason]),
            []
    end
.
%-------------------------------------------------------------------
