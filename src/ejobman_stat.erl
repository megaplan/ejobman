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
%%% of messages in a storage, writes received data to a file storage.
%%% 

-module(ejobman_stat).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).
-export([stat_t/0, stat_t/1, add_stat_t/2, upd_stat_t/3, upd_stat_t/4]).
-export([
         reload_config_signal/0
        ]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("kernel/include/file.hrl").

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
    erlang:send_after(?STAT_T, self(), periodic_check), % for redundancy
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

%% @doc returns time statistic
handle_call({stat_t, Type}, _From, St) ->
    Res = ejobman_print_stat:make_stat_t_info(St, Type),
    {reply, Res, St};

%% @doc set new debug level for facility
handle_call({set_debug_item, Facility, Level}, _From, St) ->
    % no api for this, use message passing
    New = mpln_misc_run:update_debug_level(St#est.debug, Facility, Level),
    {reply, St#est.debug, St#est{debug=New}};

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

handle_cast({add_job, Time, Tag}, St) ->
    mpln_p_debug:pr({?MODULE, 'cast add_job', ?LINE, Time, Tag},
        St#est.debug, run, 4),
    add_job_stat(Time, Tag),
    {noreply, St};

handle_cast({upd_job, Time, Tag, Work, Queued}, St) ->
    mpln_p_debug:pr({?MODULE, 'cast upd_job', ?LINE, Time, Tag},
        St#est.debug, run, 4),
    upd_job_stat(Time, Tag, Work, Queued),
    {noreply, St};

handle_cast(reload_config_signal, St) ->
    New = process_reload_config(St),
    {noreply, New};

handle_cast(_Other, St) ->
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _Other},
        St#est.debug, run, 2),
    {noreply, St}.

%------------------------------------------------------------------------------
terminate(_, State) ->
    New = do_flush(State),
    stop_storage(New),
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
%% @doc asks ejobman_stat for time statistic
%% @since 2012-02-02 14:09
%%
-spec stat_t() -> string().

stat_t() ->
    stat_t(raw).

stat_t(Type) ->
    gen_server:call(?MODULE, {stat_t, Type}).

%%-----------------------------------------------------------------------------
%%
%% @doc api call to add time statistic
%% @since 2012-02-02 14:09
%%
-spec add_stat_t(tuple(), any()) -> ok.

add_stat_t(Time, Tag) ->
    gen_server:cast(?MODULE, {add_job, Time, Tag}).

%%
%% @doc api call to update time statistic
%% @since 2012-02-02 14:09
%%
-spec upd_stat_t(any(), non_neg_integer(), non_neg_integer()) -> ok.

upd_stat_t(Tag, Work, Queued) ->
    upd_stat_t(now(), Tag, Work, Queued).

-spec upd_stat_t(tuple(), any(), non_neg_integer(), non_neg_integer()) -> ok.

upd_stat_t(Time, Tag, Work, Queued) ->
    gen_server:cast(?MODULE, {upd_job, Time, Tag, Work, Queued}).

%%-----------------------------------------------------------------------------
%%
%% @doc send a message to the server to reload own config
%% @since 2012-03-02 14:49
%%
-spec reload_config_signal() -> ok.

reload_config_signal() ->
    gen_server:cast(?MODULE, reload_config_signal).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc does all necessary preparations
%%
-spec prepare_all(#est{}) -> #est{}.

prepare_all(C) ->
    Stp = prepare_stat(C#est{start=now(), pid=self()}),
    erlang:send_after(?STAT_T, self(), periodic_check), % for redundancy
    Stp.
    
%%-----------------------------------------------------------------------------
%%
%% @doc prepares statistic
%%
prepare_stat(St) ->
    ets:new(?STAT_TAB_M, [named_table, set, protected, {keypos,1}]),
    ets:new(?STAT_TAB_H, [named_table, set, protected, {keypos,1}]),
    St.

%%-----------------------------------------------------------------------------
%%
%% @doc opens storage file
%%
prepare_storage(#est{storage_base=Base} = C) ->
    Name = mpln_misc_log:get_fname(Base),
    mpln_p_debug:pr({?MODULE, 'prepare_storage', ?LINE, Name},
                    C#est.debug, file, 2),
    filelib:ensure_dir(Name),
    case file:open(Name, [append, raw, binary]) of
        {ok, Fd} ->
            C#est{storage_fd=Fd, storage_start=now(), storage_cur_name=Name};
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'prepare_all open error', ?LINE, Reason},
                            C#est.debug, run, 0),
            C
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc flushes current data to file and closes current storage file
%%
stop_storage(#est{storage_fd=Fd} = St) ->
    do_flush(St),
    file:close(Fd).

%%-----------------------------------------------------------------------------
%%
%% @doc performs periodic checks, triggers timer for next periodic check
%%
-spec periodic_check(#est{}) -> #est{}.

periodic_check(#est{timer=Ref, clean_interval=T} = St) ->
    mpln_misc_run:cancel_timer(Ref),
    New = clean_old(St),
    Nref = erlang:send_after(T * 1000, self(), periodic_check),
    New#est{timer=Nref}.

%%-----------------------------------------------------------------------------
%%
%% @doc updates items in job statistic (minute and hourly)
%%
-spec upd_job_stat(tuple(), any(), non_neg_integer(), non_neg_integer()) ->
                          true.

upd_job_stat(Time, Tag, Work, Queued) ->
    upd_minute_job_stat(Time, Tag, Work, Queued),
    upd_hourly_job_stat(Time, Tag, Work, Queued).

upd_minute_job_stat(Time, Tag, Work, Queued) ->
    estat_misc:set_max_timed_stat(?STAT_TAB_M, 'minute', Time, {Tag, 'work'},
                                  Work),
    estat_misc:set_max_timed_stat(?STAT_TAB_M, 'minute', Time, {Tag, 'queued'},
                                  Queued).

upd_hourly_job_stat(Time, Tag, Work, Queued) ->
    estat_misc:set_max_timed_stat(?STAT_TAB_H, 'hour', Time, {Tag, 'work'},
                                  Work),
    estat_misc:set_max_timed_stat(?STAT_TAB_H, 'hour', Time, {Tag, 'queued'},
                                  Queued).


%%-----------------------------------------------------------------------------
%%
%% @doc adds item to job statistic (minute and hourly)
%%
-spec add_job_stat(tuple(), any()) -> true.

add_job_stat(Time, Tag) ->
    add_minute_job_stat(Time, Tag),
    add_hourly_job_stat(Time, Tag).

add_minute_job_stat(Time, Tag) ->
    estat_misc:add_timed_stat(?STAT_TAB_M, 'minute', Time, Tag).

add_hourly_job_stat(Time, Tag) ->
    estat_misc:add_timed_stat(?STAT_TAB_H, 'hour', Time, Tag).

%%-----------------------------------------------------------------------------
%%
%% @doc cleans old statistic and job info files
%%
-spec clean_old(#est{}) -> #est{}.

clean_old(St) ->
    clean_old_statistic(St),
    St.

clean_old_statistic(#est{stat_limit_cnt_h=Hlimit, stat_limit_cnt_m=Mlimit}) ->
    estat_misc:clean_timed_stat(?STAT_TAB_H, Hlimit),
    estat_misc:clean_timed_stat(?STAT_TAB_M, Mlimit).

%%-----------------------------------------------------------------------------
%%
%% @doc does the flushing of accumulated info to storage
%%
-spec do_flush(#est{}) -> #est{}.

do_flush(#est{storage=S} = St) ->
    List = lists:reverse(S),
    L2 = lists:map(fun(X) -> create_binary_item(St, X) end, List),
    mpln_p_debug:pr({?MODULE, 'do_flush', ?LINE, L2},
                    St#est.debug, run, 6),
    proceed_flush(St, L2).

%%-----------------------------------------------------------------------------
%%
%% @doc proceeds with flushing
%%
proceed_flush(St, []) ->
    St#est{storage=[], flush_last=now()};

proceed_flush(#est{storage_fd=Fd} = St, List) ->
    check_separator(St),
    Json = mochijson2:encode(List),
    Bin = unicode:characters_to_binary(Json),
    case file:write(Fd, Bin) of
        ok ->
            ok;
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'proceed_flush error', ?LINE, Reason},
                            St#est.debug, run, 0)
    end,
    St#est{storage=[], flush_last=now()}.

%%-----------------------------------------------------------------------------
%%
%% @doc checks whether the json field separator need to be written
%%
check_separator(#est{storage_cur_name=Name} = St) ->
    case filelib:file_size(Name) of
        0 ->
            ok;
        _ ->
            add_separator(St)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc adds the json field separator to the storage file if this file has
%% some data
%%
add_separator(#est{storage_fd=Fd} = St) ->
    case file:write(Fd, <<",\n">>) of
        ok ->
            ok;
        {error, Reason} ->
            mpln_p_debug:pr({?MODULE, 'add_separator error', ?LINE, Reason},
                            St#est.debug, run, 0)
    end.

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

%%-----------------------------------------------------------------------------
%%
%% @doc fetches config from updated environment and stores it in the state.
%%
-spec process_reload_config(#est{}) -> #est{}.

process_reload_config(St) ->
    Stf = do_flush(St),
    stop_storage(Stf),

    C = ejobman_conf:get_config_stat(),
    prepare_storage(C).

%%-----------------------------------------------------------------------------
