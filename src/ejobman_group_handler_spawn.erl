%%%
%%% ejobman_group_handler_spawn: spawns one group handler
%%%
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author arkdro <arkdro@gmail.com>
%%% @since 2012-01-10 13:16
%%% @license MIT
%%% @doc functions for creating a child that handles one job group
%%%

-module(ejobman_group_handler_spawn).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([prepare_group_handlers/1]).
-export([
         terminate_children/1
        ]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ejobman.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc terminate spawned children
%% @since 2012-03-01 17:21
%%
-spec terminate_children(#ejm{}) -> ok.

terminate_children(#ejm{group_handler_run=List}) ->
    F = fun({Pars, {ok, _Pid}}) ->
                Id = proplists:get_value(id, Pars),
                supervisor:terminate_child(ejobman_group_supervisor, Id);
           ({Pars, {ok, _Pid, _}}) ->
                Id = proplists:get_value(id, Pars),
                supervisor:terminate_child(ejobman_group_supervisor, Id);
           (_) ->
                ok
    end,
    lists:foreach(F, List).

%%-----------------------------------------------------------------------------
%%
%% @doc starts group handlers for configured groups and one for default group
%%
-spec prepare_group_handlers(#ejm{}) -> #ejm{}.

prepare_group_handlers(#ejm{group_handler=Gh, job_groups=Groups,
                            max_children=Max} = St) ->
    Def_group = #jgroup{id=?GID_DEFAULT, max_children=Max},
    List = [Def_group | Groups],
    Res = lists:map(fun(X) -> start_group_handler(Gh, X) end, List),
    mpln_p_debug:pr({?MODULE, 'prepare_group_handlers', ?LINE, Res},
                    St#ejm.debug, run, 3),
    St#ejm{group_handler_run=Res}.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc starts one group handler with given parameters. Returned value
%% unnecessary in fact, just for runtime debugging
%%
start_group_handler(Gh_params, #jgroup{id=Gid, max_children=Max}) ->
    Id = make_ref(),
    Short_params = [
                 {id, Id},
                 {group, Gid},
                 {max_children, Max}
                ],
    Ch_params = [{group_handler, Gh_params} | Short_params],
    StartFunc = {ejobman_group_handler, start_link, [Ch_params]},
    Child = {Id, StartFunc, permanent, 1000, worker, [ejobman_group_handler]},
    Res = supervisor:start_child(ejobman_group_supervisor, Child),
    {Short_params, Res}.

%%-----------------------------------------------------------------------------
