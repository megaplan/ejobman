%%%-----------------------------------------------------------------
%%% functions that do real handling of the command received by
%%% ejobman_handler
%%%-----------------------------------------------------------------
-module(ejobman_handler_cmd).
-export([do_command/3]).
-include("ejobman.hrl").
%-------------------------------------------------------------------
% @doc do command processing in background then send reply to the client
-spec do_command(#ejm{}, any(), binary()) -> #ejm{}.

do_command(St, From, Cmd) ->
    p_debug:pr({?MODULE, 'do_command', ?LINE, Cmd}, St#ejm.debug, run, 4),
    % parameters for ejobman_child
    Params = [
        {from, From},
        {cmd, Cmd},
        {debug, St#ejm.debug}
        ],
    Res = supervisor:start_child(ejobman_child_supervisor, [Params]),
    p_debug:pr({?MODULE, 'do_command', ?LINE, Res}, St#ejm.debug, run, 4),
    St
.
%-------------------------------------------------------------------
