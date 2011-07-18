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

%store_rabbit_cmd(#ejm{rses=#rses{sep=Sep}} = State,
%        <<"cmd", Sep:2/binary-unit:8, Rest/binary>>) ->
%    p_debug:pr({?MODULE, 'store_rabbit_cmd cmd', ?LINE, Rest},
%        State#ejm.debug, run, 4),
%    send_cmd(State, Rest),
%    State
%;
store_rabbit_cmd(State, Bin) ->
    p_debug:pr({?MODULE, 'store_rabbit_cmd json', ?LINE, Bin},
        State#ejm.debug, run, 4),
    case catch mochijson2:decode(Bin) of
        {'EXIT', Reason} ->
            p_debug:pr({?MODULE, 'store_rabbit_cmd error', ?LINE, Reason},
                State#ejm.debug, run, 2);
        Data ->
            send_json_cmd(State, Data)
    end,
    State
.
%-------------------------------------------------------------------
send_cmd(_State, Rest) ->
    % should we use catch, timeout, etc... ?
    ejobman_handler:cmd(<<"head">>, Rest)
.
%-------------------------------------------------------------------
-spec send_json_cmd(#ejm{}, any()) -> ok.

send_json_cmd(State, Data) ->
    Type = misc_json:get_type(Data),
    proceed_cmd_type(State, Type, Data).
%-------------------------------------------------------------------
-spec proceed_cmd_type(#ejm{}, binary(), any()) -> ok.

proceed_cmd_type(State, <<"rest">>, Data) ->
    Info = misc_json:get_job_info(Data),
    Method = misc_json:get_method(Info),
    Url = misc_json:get_url(Info),
    % timeout on child crash leads to exception
    Res = (catch ejobman_handler:cmd(Method, Url)),
    p_debug:pr({?MODULE, 'proceed_cmd_type res', ?LINE, Res},
        State#ejm.debug, run, 5);
proceed_cmd_type(State, Other, _Data) ->
    p_debug:pr({?MODULE, 'proceed_cmd_type other', ?LINE, Other},
        State#ejm.debug, run, 2).
%-------------------------------------------------------------------
