%%%-----------------------------------------------------------------
%%% functions that do real handling of the command received by
%%% ejobman_handler
%%%-----------------------------------------------------------------
-module(ejobman_handler_cmd).
-export([do_command/4]).
-include("ejobman.hrl").
%-------------------------------------------------------------------
% @doc do command processing in background then send reply to the client
-spec do_command(#ejm{}, any(), binary(), binary()) -> #ejm{}.

do_command(St, From, Method, Url) ->
    p_debug:pr({?MODULE, 'do_command cmd', ?LINE, Method, Url},
        St#ejm.debug, run, 4),
    % parameters for ejobman_child
    Params = [
        {from, From},
        {method, Method},
        {url, Url},
        {debug, St#ejm.debug}
        ],
    Res = supervisor:start_child(ejobman_child_supervisor, [Params]),
    p_debug:pr({?MODULE, 'do_command res', ?LINE, Res}, St#ejm.debug, run, 4),
    St
.
%-------------------------------------------------------------------
