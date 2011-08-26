%%% 
%%% ejobman_worker_spawn: spawns one ejobman worker
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
%%% @since 2011-07-15 10:00
%%% @license MIT
%%% @doc spawns one ejobman worker
%%% 
-module(ejobman_worker_spawn).
-export([spawn_one_worker/2]).
-include("ejobman.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc Spawns a new worker, stores its pid (and a ref) in a list,
%% returns the modified pool.
%% @since 2011-07-21 18:00
%%
-spec spawn_one_worker(#ejm{}, #pool{}) -> {reference() | error, #pool{}}.

spawn_one_worker(C, Pool) ->
    Id = make_ref(),
    Child_config = make_child_config(Pool, Id),
    StartFunc = {ejobman_long_worker, start_link, [Child_config]},
    % for 'permanent' restart policy either worker or handler must contact
    % one another so handler keeps actual list of children. Or use gproc...
    % In the case of 'temporary' the handler does all the housekeeping
    Child = {Id, StartFunc, temporary, 1000, worker, [ejobman_long_worker]},
    Workers = Pool#pool.workers,
    Res = supervisor:start_child(ejobman_long_supervisor, Child),
    mpln_p_debug:pr({?MODULE, 'real_spawn_one_worker res', ?LINE, Res},
        C#ejm.debug, run, 3),
    case Res of
        {ok, Pid} ->
            Mref = erlang:monitor(process, Pid),
            Ch = #chi{pid=Pid, id=Id, start=now(), mon=Mref},
            {Id, Pool#pool{workers = [Ch | Workers]}};
        {ok, Pid, _Info} ->
            Mref = erlang:monitor(process, Pid),
            Ch = #chi{pid=Pid, id=Id, start=now(), mon=Mref},
            {Id, Pool#pool{workers = [Ch | Workers]}};
        {error, _Reason} ->
            {error, Pool}
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc creates config (proplist actually) for a child
%% @since 2011-07-21 18:00
%%
-spec make_child_config(#pool{}, reference()) -> list().

make_child_config(Pool, Ref) ->
    [{id, Ref} | Pool#pool.worker_config]
.
%%-----------------------------------------------------------------------------
