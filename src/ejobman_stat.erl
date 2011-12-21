%%%
%%% ejobman_stat: jobs statistic server
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
%%% @since 2011-12-20 12:45
%%% @license MIT
%%% @doc Receives messages with jobs workflow data, keeps a limited number
%%% of messages in a storage.
%%% 

-module(ejobman_stat).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).
-export([add/2, add/3, add/5]).
-export([get/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("estat.hrl").
-include("job.hrl").
-include("amqp_client.hrl").

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(_) ->
    C = ejobman_conf:get_config_stat(),
    New = prepare_all(C),
    process_flag(trap_exit, true), % to save storage
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE}, New#est.debug, run, 1),
    {ok, New, ?STAT_T}.

%------------------------------------------------------------------------------
-spec handle_call(any(), any(), #est{}) ->
                         {stop, normal, ok, #est{}}
                             | {reply, any(), #est{}}.
%%
%% Handling call messages
%% @since 2011-12-20 13:34
%%
handle_call(stop, _From, St) ->
    {stop, normal, ok, St};

handle_call(status, _From, St) ->
    {reply, St, St};

handle_call({get, Start, Stop}, _From, St) ->
    mpln_p_debug:pr({?MODULE, get1, ?LINE, Start, Stop}, St#est.debug, run, 2),
    Res = get_items(St, Start, Stop),
    {reply, Res, St};

handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, other, ?LINE, _N}, St#est.debug, run, 2),
    {reply, {error, unknown_request}, St}.

%------------------------------------------------------------------------------
-spec handle_cast(any(), #est{}) -> any().
%%
%% Handling cast messages
%% @since 2011-12-20 13:34
%%
handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast({add, Id, Time_info, Data}, St) ->
    mpln_p_debug:pr({?MODULE, 'cast add', ?LINE, Id, Time_info},
        St#est.debug, run, 3),
    New = add_item(St, Id, Time_info, Data),
    {noreply, New};

handle_cast(_Other, St) ->
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _Other},
        St#est.debug, run, 2),
    {noreply, St}.

%------------------------------------------------------------------------------
terminate(_, State) ->
    flush_storage(State),
    mpln_p_debug:pr({?MODULE, terminate, ?LINE}, State#est.debug, run, 1),
    ok.

%------------------------------------------------------------------------------
-spec handle_info(any(), #est{}) -> any().
%%
%% Handling all non call/cast messages
%%
handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, 'info_timeout', ?LINE}, State#est.debug, run, 3),
    New = periodic_check(State),
    {noreply, New};

handle_info(periodic_check, State) ->
    mpln_p_debug:pr({?MODULE, 'info_periodic_check', ?LINE},
                    State#est.debug, run, 6),
    New = periodic_check(State),
    {noreply, New};

handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, 'info_other', ?LINE, _Req},
                    State#est.debug, run, 2),
    {noreply, State}.

%------------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
-spec start() -> any().
%%
%% @doc starts receiver gen_server
%% @since 2011-12-30 13:34
%%
start() ->
    start_link().

%%-----------------------------------------------------------------------------
%%
%% @doc starts receiver gen_server with pre-defined config
%% @since 2011-12-20 13:34
%%
-spec start_link() -> any().

start_link() ->
    start_link(?CONF).

%%
%% @doc starts receiver gen_server with given config
%% @since 2011-12-20 13:34
%%
-spec start_link(string()) -> any().

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%-----------------------------------------------------------------------------
%%
%% @doc stops receiver gen_server
%% @since 2011-12-20 13:34
%%
-spec stop() -> any().

stop() ->
    gen_server:call(?MODULE, stop).

%%-----------------------------------------------------------------------------
%%
%% @doc sends input id, time, data to the server to store them in the storage
%% @since 2011-12-20 19:00
%%
-spec add(any(), any()) -> ok.

add(Id, Data) ->
    add(Id, undefined, Data).

-spec add(any(), any(), any()) -> ok.

add(Id1, Id2, Data) ->
    Now = now(),
    add(Id1, Id2, mpln_misc_time:get_gmt_time(Now), Now, Data).

-spec add(any(), any(), non_neg_integer(), tuple(), any()) -> ok.

add(Id1, Id2, Time, Now, Data) ->
    List = make_list(Data),
    gen_server:cast(?MODULE, {add, {Id1, Id2}, {Time, Now}, List}).

%%-----------------------------------------------------------------------------
%%
%% @doc receives start/stop times and returns json binary with stat items
%% for this interval. Times are unix times, namely seconds from 1970-01-01
%% @since 2011-12-20 19:03
%%
-spec get(string(), string()) -> binary().

get(Start, Stop) ->
    gen_server:call(?MODULE, {get, Start, Stop}).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc does all necessary preparations: [re]opens log file.
%% @since 2011-07-15
%%
-spec prepare_all(#est{}) -> #est{}.

prepare_all(C) ->
    prepare_storage(C).

%%-----------------------------------------------------------------------------
%%
%% @doc prepares storage by reading existing table or creating new table.
%% @since 2011-07-15
%%
-spec prepare_storage(#est{}) -> #est{}.

prepare_storage(#est{storage=File} = C) ->
    case file:read_file_info(File) of
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'prepare_storage', ?LINE, 'error',
                             Reason}, C#est.debug, run, 1),
            Tid = ets:new(ejobman_stat, [set, protected, named_table,
                                         {keypos, 1}]),
            C#est{tid=Tid};
        {ok, _Info} ->
            read_storage(C)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc reads an existing storage file or create a new storage in case
%% of an error
%%
-spec read_storage(#est{}) -> #est{}.

read_storage(#est{storage=File} = C) ->
    case ets:file2tab(File, [{verify, false}]) of
        {ok, Tab} ->
            C#est{tid=Tab};
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'read_storage', ?LINE, 'error', Reason},
                            C#est.debug, run, 1),
            Tid = ets:new(ejobman_stat, [set, protected, named_table,
                                         {keypos, 1}]),
            C#est{tid=Tid}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc saves storage to disk
%%
-spec flush_storage(#est{}) -> ok.

flush_storage(#est{storage=File, tid=Tab} = St) ->
    Tname = File ++ ".tmp",
    case ets:tab2file(Tab, Tname) of
        ok ->
            rename_tabs(St, Tname);
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'flush_storage', ?LINE, 'error', Reason},
                            St#est.debug, run, 0)
    end.

%%-----------------------------------------------------------------------------
rename_tabs(#est{storage=File} = St, Tname) ->
    case file:rename(Tname, File) of
        ok ->
            ok;
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'rename_tabs', ?LINE, 'error', Reason},
                            St#est.debug, run, 0),
            file:delete(Tname)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc deletes old entries from the storage
%%
-spec clean_storage(#est{}) -> ok.

clean_storage(#est{tid=Tab, keep_time=Time} = St) ->
    % item: {ref, stage}, time (gregorian seconds), time (now), data
    Now = mpln_misc_time:get_gmt_time(),
    Mhead = {'_', '$1', '_', '_'},
    T2 = Now - Time,
    Guards = [{'<', '$1', T2}],
    Mres = [true],
    Mfun = {Mhead, Guards, Mres},
    Mspec = [Mfun],
    mpln_p_debug:pr({?MODULE, 'clean_storage before', ?LINE, ets:info(Tab)},
                    St#est.debug, storage, 4),
    Del = ets:select_delete(Tab, Mspec),
    mpln_p_debug:pr({?MODULE, 'clean_storage after', ?LINE, Del, ets:info(Tab)},
                    St#est.debug, storage, 3),
    ok.

%%-----------------------------------------------------------------------------
%%
%% @doc performs periodic checks, triggers timer for next periodic check
%%
-spec periodic_check(#est{}) -> #est{}.

periodic_check(#est{timer=Ref, flush_interval=T} = St) ->
    case Ref of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(Ref)
    end,
    clean_storage(St),
    flush_storage(St),
    Nref = erlang:send_after(T * 1000, self(), periodic_check),
    St#est{timer=Nref}.

%%-----------------------------------------------------------------------------
%%
%% @doc stores id, time, data in the storage
%%
-spec add_item(#est{}, any(), {non_neg_integer(), tuple()}, any()) -> #est{}.

add_item(#est{tid=Tid} = St, Id, {Time, Now}, Data) ->
    ets:insert(Tid, {Id, Time, Now, Data}),
    St.

%%-----------------------------------------------------------------------------
%%
%% @doc tries to maintain a proper list containing {key, value} tuples. This
%% list is to be stored in a storage.
%%
make_list(undefined) ->
    undefined;

make_list({_, _} = Data) ->
    [Data];

make_list(Data) ->
    Data.

%%-----------------------------------------------------------------------------
%%
%% @doc receives start/stop times and returns json binary with stat items
%% for this interval. Times are unix times, namely seconds from 1970-01-01,
%% local time zone.
%%
-spec get_items(#est{}, non_neg_integer(), non_neg_integer()) -> binary().

get_items(#est{tid=Tab} = St, Start, Stop) ->
    T1 = mpln_misc_time:make_gmt_gregorian_seconds(Start),
    T2 = mpln_misc_time:make_gmt_gregorian_seconds(Stop),
    % item: {ref, stage}, time (gregorian seconds), time (now), data
    Mhead = {'_', '$1', '_', '_'},
    Guards = [{'>', '$1', T1}, {'<', '$1', T2}],
    Mres = ['$_'],
    Mfun = {Mhead, Guards, Mres},
    Mspec = [Mfun],
    List = ets:select(Tab, Mspec),
    mpln_p_debug:pr({?MODULE, 'get_items', ?LINE, Start, Stop, T1, T2,
                     length(List)}, St#est.debug, run, 3),
    create_binary_response(St, List).

%%-----------------------------------------------------------------------------
%%
%% @doc creates json binary from the fetched item list
%%
-spec create_binary_response(#est{}, list()) -> binary().

create_binary_response(St, List) ->
    mpln_p_debug:pr({?MODULE, 'create_binary_response', ?LINE, List},
                    St#est.debug, run, 5),
    L2 = lists:map(fun(X) -> create_binary_item(St, X) end, List),
    mpln_p_debug:pr({?MODULE, 'create_binary_response', ?LINE, L2},
                    St#est.debug, run, 6),
    Json = mochijson2:encode(L2),
    unicode:characters_to_binary(Json).

%%-----------------------------------------------------------------------------
%%
%% @doc creates json binary from the fetched item list
%%
-spec create_binary_item(#est{}, tuple()) -> list().

create_binary_item(_St, {{Ref, Stage}, _Time, Now, undefined}) ->
    [
     {<<"ref">>, mpln_misc_web:make_binary(Ref)},
     {<<"stage">>, Stage},
     {<<"time">>, list_to_binary(mpln_misc_time:get_time_str_us(Now))}
    ];

create_binary_item(_St, {{Ref, Stage}, _Time, Now, List}) ->
    [
     {<<"ref">>, mpln_misc_web:make_binary(Ref)},
     {<<"stage">>, Stage},
     {<<"time">>, list_to_binary(mpln_misc_time:get_time_str_us(Now))},
     {<<"params">>, List}
    ];

create_binary_item(St, _Item) ->
    mpln_p_debug:pr({?MODULE, 'create_binary_item unknown', ?LINE, _Item},
                    St#est.debug, run, 4),
    [].

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).

create_test_storage() ->
    Tab = ejobman_stat_test,
    catch ets:delete(Tab),
    ets:new(Tab, [set, protected, named_table, {keypos, 1}]),
    ets:delete_all_objects(Tab),
    Tab.

export_storage(#est{tid=Tid}) ->
    L = ets:tab2list(Tid),
    lists:keysort(1, L).

rand_delay(N) when is_integer(N) andalso N >= 1 ->
    R = random:uniform(N),
    timer:sleep(R);
rand_delay(_) ->
    ok.

fill2_test_storage(St) ->
    add_item(St, {123456, fill2}, 1, "test2").
    
fill_test_storage(#est{tid=T}= St) ->
    fill_test_storage(#est{tid=T}= St, 0).

fill_test_storage(St, Delay) ->
    R1 = make_ref(),
    R2 = make_ref(),
    L = [
         {R1, push_to_rabbit, "json"},
         {R1, fetch_from_rabbit, "queue"},
         {R1, http_start, "none"},
         {R1, http_stop, "empty"},

         {R2, push_to_rabbit, "json"},
         {R2, fetch_from_rabbit, "queue"},
         {R2, http_start, "none"},
         {R2, http_stop, "empty"}
        ],
    F = fun({Id, Stage, Data}) ->
                rand_delay(Delay),
                Time = mpln_misc_time:get_gmt_time(),
                add_item(St, {Id, Stage}, Time, Data)
        end,
    lists:foreach(F, L).

% @doc test for flush_storage and read_storage
flush_read_storage_test() ->
    Tid = create_test_storage(),
    File = "/tmp/ejobman_stat_test.dat",
    St = #est{tid=Tid, keep_time=1, debug=[], storage=File},
    fill_test_storage(St),
    D0 = export_storage(St),
    flush_storage(St),
    ets:delete(Tid),
    read_storage(St),
    D1 = export_storage(St),
    %?debugFmt("flush_storage_test:~n~p~n~p~n", [D0, D1]),
    file:delete(File),
    ?assert(D0 =:= D1).

% @doc test for clean_storage
clean_storage_test() ->
    Tid = create_test_storage(),
    St = #est{tid=Tid, keep_time=1, debug=[]},
    D0 = export_storage(St),
    fill_test_storage(St),
    timer:sleep(2100),
    clean_storage(St),
    D2 = export_storage(St),
    %?debugFmt("clean_storage_test cleaned:~n~p~n", [D2]),
    ?assert(D0 =:= D2).

% @doc test for clean_storage
clean2_storage_test() ->
    Tid = create_test_storage(),
    St = #est{tid=Tid, keep_time=2, debug=[]},
    fill_test_storage(St),
    D0 = export_storage(St),
    fill2_test_storage(St),
    export_storage(St),
    clean_storage(St),
    D2 = export_storage(St),
    %?debugFmt("clean_storage_test:~n~p~n~p~n~p~n", [D0, D1, D2]),
    ?assert(D0 =:= D2).

-endif.
%%-----------------------------------------------------------------------------
