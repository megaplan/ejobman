%%%-----------------------------------------------------------------
%%% functions that do real handling of the payload received via AMQP
%%%-----------------------------------------------------------------
-module(ejobman_receiver_cmd).
-export([store_rabbit_cmd/2]).
-include("ejobman.hrl").
-include("rabbit_session.hrl").
-define(HTTP_TIMEOUT, 15000).
%-------------------------------------------------------------------
% @doc sends received command to command handler. Returns nothing actually.
-spec store_rabbit_cmd(#ejm{}, binary()) -> #ejm{}.

store_rabbit_cmd(#ejm{rses=#rses{sep=Sep}} = State,
        <<"cmd", Sep:2/binary-unit:8, Rest/binary>>) ->
    p_debug:pr({?MODULE, store_rabbit_cmd, ?LINE, cmd, Rest},
        State#ejm.debug, run, 4),
    send_cmd(State, Rest),
    State
;
store_rabbit_cmd(State, Payload) ->
    p_debug:pr({?MODULE, store_rabbit_cmd, ?LINE, Payload},
        State#ejm.debug, run, 4),
    Url = binary_to_list(Payload),
    Res = http:request(head, {Url, []},
        [{timeout, ?HTTP_TIMEOUT}, {connect_timeout, ?HTTP_TIMEOUT}],
        []),
    p_debug:p("~p:store_rabbit_cmd:~p http result:~n~p~n",
        [?MODULE, ?LINE, Res], State#ejm.debug, run, 3),
    State.
%-------------------------------------------------------------------
send_cmd(State, Rest) ->
    % should we use catch, timeout, etc... ?
    ejobman_handler:cmd(Rest)
.
%-------------------------------------------------------------------
