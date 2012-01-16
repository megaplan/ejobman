%%%
%%% ejobman_log: log job info
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
%%% @since 2011-10-20 11:35
%%% @license MIT
%%% @doc job logging functions. Log parsing and rss recreating. Forced by
%%% some contradictious customer requests.
%%% 

-module(ejobman_log).
%-export([log_job/2, log_job_result/3]).
-export([make_jlog_xml/1, make_jlog_xml/2]).
-export([get_last_jobs/0, get_last_jobs/1, get_last_jobs/2]).
-export([get_last_jobs_rss/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/file.hrl").
-include("ejobman.hrl").
-include("job.hrl").

%%%----------------------------------------------------------------------------
%%% Defines
%%%----------------------------------------------------------------------------

-define(NJOBS, 100).
-define(BLOCK, 4096).
-define(QRY, request).
-define(RES, result).
-record(item, {
    ref,
    date,
    desc,
    status
}).

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc creates rss with last jobs
%% @since 2011-12-19 17:45
%%
-spec get_last_jobs_rss(#ejm{}, non_neg_integer()) -> binary().

get_last_jobs_rss(#ejm{stat_r=Stat} = St, N) ->
    List = cut_job_list(Stat, N),
    make_job_rss(St, List)
.

%%-----------------------------------------------------------------------------
%%
%% @doc creates an output page with last jobs
%% @since 2011-12-19 13:55
%%
-spec get_last_jobs() -> string().

get_last_jobs() ->
    get_last_jobs('html').

-spec get_last_jobs('html' | 'text' | 'rss') -> string().

get_last_jobs(Type) ->
    get_last_jobs(Type, ?NJOBS).

-spec get_last_jobs('html' | 'text' | 'rss', non_neg_integer()) -> string().

get_last_jobs(Type, X) ->
    Stype = mpln_misc_web:make_string(Type),
    N = case X of
            _ when is_integer(X) andalso X > 0 ->
                X;
            _ ->
                ?NJOBS
        end,
    List = get_job_list(N),
    make_job_output(Stype, List).

%%-----------------------------------------------------------------------------
%%
%% @doc reads last N bytes from job log file and creates the xml (rss) output
%% @since 2011-10-21 14:07
%%
-spec make_jlog_xml(string()) -> string().

make_jlog_xml(File) ->
    make_jlog_xml(File, ?BLOCK).

-spec make_jlog_xml(string(), non_neg_integer()) -> string().

make_jlog_xml(File, Size) ->
    Data = get_jlog_data(File, Size),
    Head = get_jlog_head(),
    Foot = get_jlog_foot(),
    Head ++ Data ++ Foot.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc returns rss message header
%%
msg_head() ->
    "<item>\n".

%%-----------------------------------------------------------------------------
%%
%% @doc returns rss message footer
%%
msg_foot() ->
    "</item>\n".

%%-----------------------------------------------------------------------------
%%
%% @doc returns rss message date
%%
msg_date() ->
    Lt = erlang:localtime(),
    Ts = httpd_util:rfc1123_date(Lt),
    Beg = "<pubDate>",
    End = "</pubDate>\n",
    [Beg, Ts, End].

%%-----------------------------------------------------------------------------
%%
%% @doc creates rss message description with job result body
%% in dependence of the configured debug level
%%
-spec make_msg_result_body(#ejm{}, tuple()) ->
    string() | {string(), string(), binary()}.

make_msg_result_body(#ejm{debug=D}, {Status, Hdr, Body}) ->
    N = proplists:get_value(job_result, D, 0),
    % N=3 seems most useful level
    if
        N >= 5, is_binary(Body) ->
            Beg_str = io_lib:format("status=~p~nheaders=~p~n", [Status, Hdr]),
            Body_str = io_lib:format("body_str=~p~n", [Body]),
            {Beg_str, Body_str, Body};
        N >= 5, is_list(Body) ->
            Bin = unicode:characters_to_binary(Body),
            Beg_str = io_lib:format("status=~p~nheaders=~p~n", [Status, Hdr]),
            Body_str = io_lib:format("body_str=~p~n", [Body]),
            {Beg_str, Body_str, Bin};
        N >= 4 ->
            io_lib:format("status=~p~nheaders=~p~nbody=~p~n",
                [Status, Hdr, Body]);
        N >= 3, is_binary(Body) ->
            Str = io_lib:format("status=~p~nheaders=~p~n",
                [Status, Hdr]),
            {Str, "", Body};
        N >= 3, is_list(Body) ->
            Bin = unicode:characters_to_binary(Body),
            Str = io_lib:format("status=~p~nheaders=~p~n",
                [Status, Hdr]),
            {Str, "", Bin};
        N >= 2 ->
            io_lib:format("status=~p~nheaders=~p~n", [Status, Hdr]);
        N >= 1 ->
            io_lib:format("status=~p~n", [Status]);
        true ->
            ""
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc creates short debug info from a job
%%
make_short_info(#job{} = J) ->
    io_lib:format("id=~p~ntype=~p~ngroup=~p~n"
        "method=~p~nurl=~p~nhost=~p~n", [
            J#job.id,
            J#job.type,
            J#job.group,
            J#job.method,
            J#job.url,
            J#job.host
        ]);

make_short_info(J) ->
    % happened once, so it was interesting to see - what had come...
    io_lib:format("unknown record:~n~p~n", [J]).

%%-----------------------------------------------------------------------------
%%
%% @doc returns rss message description with job result body
%%
-spec msg_res_body(#ejm{}, tuple()) -> list().

msg_res_body(St, R) ->
    Text1 = "<description><![CDATA[",
    Text2 =
        case R of
            {ok, {_, _, _}=Result} ->
                make_msg_result_body(St, Result);
            {error, Reason} ->
                make_msg_result_body(St, {error, [], Reason});
            {Code, Body} ->
                make_msg_result_body(St, {Code, [], Body})
        end,
    Text3 =
        case Text2 of
            {Beg_str, Body_str, Body_bin} ->
                [Beg_str, "\nbody_bin=", Body_bin, "\n", Body_str, "\n"];
            _ ->
                Text2
        end,
    Text4 = "]]></description>\n",
    [Text1, Text3, Text4].

%%-----------------------------------------------------------------------------
%%
%% @doc reads N bytes from file and removes leading rubbish
%%
-spec get_jlog_data(string(), non_neg_integer()) -> string().

get_jlog_data(File, Size) ->
    Res = prepare_fd(File),
    Data = read_data(Res, Size),
    binary_to_list(Data).

%%-----------------------------------------------------------------------------
%%
%% @doc sets position in file based on file size and requested size.
%% Returns size to read.
%%
get_pos_len(Fd, #file_info{size = Size}, Size_in)
        when is_integer(Size_in), Size > Size_in, Size_in > 0 ->
    file:position(Fd, {eof, -Size_in}),
    Size_in;
get_pos_len(Fd, #file_info{size = Size}, Size_in) when Size > Size_in ->
    file:position(Fd, {eof, -?BLOCK}),
    ?BLOCK;
get_pos_len(Fd, #file_info{size = Size}, _Size_in) ->
    file:position(Fd, bof),
    Size.

%%-----------------------------------------------------------------------------
%%
%% @doc reads tail of the log, strips non xml rubbish at the beginning,
%% joins request-result pairs, returns binary data
%%
-spec read_data({ok, any(), #file_info{}, non_neg_integer()}
    | {error, any(), any()}, non_neg_integer()) -> binary().

read_data({ok, Fd, Info}, Size_in) ->
    Size = get_pos_len(Fd, Info, Size_in),
    case file:read(Fd, Size) of
        {ok, Data} ->
            file:close(Fd),
            Sdata = strip_rss_begin(Data),
            build_new_rss(Sdata);
        {error, Reason} ->
            error_logger:info_report({?MODULE, read_data, ?LINE,
                error, Reason}),
            file:close(Fd),
            <<>>
    end;
read_data({error, _R1, _R2}, _) ->
    <<>>.

%%-----------------------------------------------------------------------------
%%
%% @doc reads file info, opens file, returns file handler and info or error.
%%
-spec prepare_fd(string()) -> {ok, any(), #file_info{}}
    | {error, any(), any()}.

prepare_fd(File) ->
    case file:read_file_info(File) of
        {ok, Info} ->
            case file:open(File, [raw, binary]) of
                {ok, Fd} ->
                    {ok, Fd, Info};
                {error, R_p} ->
                    error_logger:info_report({?MODULE, prepare_fd, ?LINE,
                        error_open, R_p}),
                    {error, R_p, Info}
            end;
        {error, R_i} ->
            error_logger:info_report({?MODULE, prepare_fd, ?LINE,
                error_info, R_i}),
            {error, R_i, undefined}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc strip leading non xml rubbish.
%%
-spec strip_rss_begin(binary()) -> binary().

strip_rss_begin(Data) ->
    case re:run(Data, "<item>") of
        {match, [{Offset, _}]} ->
            {_, D2} = split_binary(Data, Offset),
            D2;
        _ ->
            Data
    end.

%%-----------------------------------------------------------------------------
get_jlog_head() ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        "<rss version=\"2.0\">\n"
        "<channel>\n"
.

%%-----------------------------------------------------------------------------
get_jlog_foot() ->
    "</channel>\n</rss>\n\n"
.

%%-----------------------------------------------------------------------------
%%
%% @doc builds new rss data
%%
build_new_rss(Data) ->
    case re:run(Data, "(<item>.*</item>)", [global, dotall, ungreedy, {capture, all_but_first, binary}]) of
        {match, L} ->
            Dict = parse_items(L),
            List = create_new_items(Dict),
            create_new_data(List);
        _ ->
            Data
    end.

%%-----------------------------------------------------------------------------
parse_items(List) ->
    D = orddict:new(),
    lists:foldl(fun get_info_lines/2, D, List).

%%-----------------------------------------------------------------------------
-spec get_info_lines(binary(), any()) -> any().

get_info_lines(Item, Acc) ->
    % Item is [Item] in fact because re:run returns so for 'global'.
    % Key: for later use in sort. So don't parse awry rfc822 date
    Key = get_ref_longnum(Item),
    Ref = get_ref(Item),
    Date = get_date(Item),
    Desc = get_description(Item),
    Desc_c = clear_extra(Desc),
    Desc_p = clear_pre(Desc_c),
    St = get_status(Desc_p),
    Data = fetch_item(Key, Acc, Desc_p),
    New = Data#item{
        ref = Ref,
        date = Date,
        status = St
    },
    orddict:store(Key, New, Acc).

%%-----------------------------------------------------------------------------
fetch_item(Key, Acc, Desc) ->
    case orddict:find(Key, Acc) of
        {ok, Val} ->
            Val;
        _ ->
            #item{desc=Desc}
    end.

%%-----------------------------------------------------------------------------
get_status(Item) ->
    case re:run(Item, "\\bstatus=([^<>\\[\\]]+)", [{capture, all_but_first, binary}]) of
        {match, L} ->
            hd(L);
        _ ->
            <<"status_undefined">>
    end.

%%-----------------------------------------------------------------------------
get_description(Item) ->
    case re:run(Item, "<description>(.+?)</description>", [dotall, ungreedy, {capture, all_but_first, binary}]) of
        {match, L} ->
            clear_extra(hd(L));
        _ ->
            <<"description_undefined">>
    end.

%%-----------------------------------------------------------------------------
get_date(Item) ->
    case re:run(Item, "<pubDate>([^<>]+)</pubDate>", [{capture, all_but_first, binary}]) of
        {match, L} ->
            hd(L);
        _ ->
            <<"pubdate_undefined">>
    end.

%%-----------------------------------------------------------------------------
get_ref_longnum(Item) ->
    case re:run(Item, "#Ref<(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)>", [{capture, all_but_first, list}]) of
        {match, L} ->
            make_ref_text(L);
        _ ->
            <<"ref_undefined">>
    end.

%%-----------------------------------------------------------------------------
make_ref_text(List) ->
    S = lists:map(fun(X) ->
        io_lib:format("~6.10.0B", [list_to_integer(X)])
    end, List),
    list_to_binary(S).

%%-----------------------------------------------------------------------------
get_ref(Item) ->
    case re:run(Item, "(#Ref<\\d+\\.\\d+\\.\\d+\\.\\d+>)", [{capture, all_but_first, binary}]) of
        {match, L} ->
            hd(L);
        _ ->
            <<"ref_undefined">>
    end.

%%-----------------------------------------------------------------------------
clear_extra(Item) ->
    case re:run(Item, "<!\\[CDATA\\[(.*)\\]\\]>", [dotall, {capture, all_but_first, binary}]) of
        {match, L} ->
            hd(L);
        _ ->
            Item
    end.

%%-----------------------------------------------------------------------------
clear_pre(Item) ->
    case re:run(Item, "<pre>(.*)</pre>", [dotall, {capture, all_but_first, binary}]) of
        {match, L} ->
            hd(L);
        _ ->
            Item
    end.

%%-----------------------------------------------------------------------------
create_new_items(Dict) ->
    D2 = orddict:map(fun create_one_item/2, Dict),
    [V || {_K, V} <- lists:reverse(orddict:to_list(D2))].

%%-----------------------------------------------------------------------------
create_new_data(List) ->
    L2 = [[X,"\n"] || X <- List],
    iolist_to_binary(L2).

%%-----------------------------------------------------------------------------
create_one_item(_Key, I) ->
    Title = create_item_title(I),
    Date = create_item_date(I),
    Desc = create_item_description(I),
    iolist_to_binary([
        "<item>",
        Title,
        Date,
        Desc,
        "</item>\n"
    ]).

%%-----------------------------------------------------------------------------
create_item_description(I) ->
    ["<description><![CDATA[<pre>",
    I#item.desc,
    "</pre>]]></description>\n"].

%%-----------------------------------------------------------------------------
create_dur_item(Tag, T2, T1) ->
    Dur = timer:now_diff(T2, T1),
    Str = io_lib:format("~.3f", [Dur/1000.0]),
    [
        "<", Tag, ">",
        Str,
        "</", Tag, ">\n"
    ].

%%-----------------------------------------------------------------------------
create_time_item(Tag, undefined) ->
    [
        "<", Tag, ">",
        "</", Tag, ">\n"
    ];

create_time_item(Tag, Time) ->
    Str = mpln_misc_time:get_time_str_us(Time),
    [
        "<", Tag, ">",
        Str,
        "</", Tag, ">\n"
    ].

%%-----------------------------------------------------------------------------
create_path_item(Path) ->
    [
        "<author>",
        Path,
        "</author>\n"
    ].

%%-----------------------------------------------------------------------------
create_item_date(I) ->
    ["<pubDate>",
    I#item.date,
    "</pubDate>\n"].

%%-----------------------------------------------------------------------------
create_item_title(I) ->
    ["<title><![CDATA[Job - ",
    I#item.status,
    " - ",
    I#item.ref,
    "]]></title>\n"].

%%-----------------------------------------------------------------------------
%%
%% @doc fetches last job statistic from ejobman_handler
%%
-spec get_job_list(non_neg_integer()) -> [{reference(), #jst{}}].

get_job_list(N) ->
    Stat = ejobman_handler:stat_r(),
    cut_job_list(Stat, N).

%%-----------------------------------------------------------------------------
%%
%% @doc cut the job list to N
%%
-spec cut_job_list(dict(), non_neg_integer()) -> [{reference(), #jst{}}].

cut_job_list(Stat, N) ->
    Size = dict:size(Stat),
    L = dict:to_list(Stat),
    L2 = lists:keysort(1, L),
    L3 =
        if  N =< Size ->
                lists:sublist(L2, Size - N + 1, Size);
            true ->
                L2
        end,
    %lists:reverse(L3)
    L3.

%%-----------------------------------------------------------------------------
%%
%% @doc creates text from list of jobs
%%
-spec make_job_rss(#ejm{}, [{reference(), #jst{}}]) -> binary().

make_job_rss(St, List) ->
    Body = [make_one_jst_rss(St, X) || X <- List],
    Head = get_jlog_head(),
    Foot = get_jlog_foot(),
    unicode:characters_to_binary([Head, Body, Foot]).

%%-----------------------------------------------------------------------------
%%
%% @doc creates text from list of jobs
%%
-spec make_job_html([{reference(), #jst{}}]) -> string().

make_job_html(List) ->
    L2 = [make_one_jst_html(X) || X <- List],
    Head = "<html><body>\n",
    Foot = "</body></html>\n",
    Body =
        case L2 of
            [] ->
                "no jobs\n";
            _ ->
                ["<p><table ", ?TABC, ">", L2, "</table>\n</p>\n"]
        end,
    lists:flatten([Head, Body, Foot]).

%%-----------------------------------------------------------------------------
%%
%% @doc creates text from list of jobs
%%
-spec make_job_text([{reference(), #jst{}}]) -> string().

make_job_text(List) ->
    L2 = [make_one_jst_text(X) || X <- List],
    lists:flatten(L2).

%%-----------------------------------------------------------------------------
%%
%% @doc creates an rss item from a data item of type {reference(), #jst{}}.
%% Output durations are in milliseconds
%%
-spec make_one_jst_rss(#ejm{}, {reference(), #jst{}}) -> list().

make_one_jst_rss(St, {Id, #jst{job=J,
        t_start_child=Start_c, t_stop_child=Stop_c,
        t_start_req=Start_r, t_stop_req=Stop_r,
        start=Start, result_full=Res}}) ->
    mpln_p_debug:pr({?MODULE, 'make_one_jst_rss', ?LINE, Id},
        St#ejm.debug, run, 4),
    Path_str = create_path_item(J#job.path),
    Start_str = create_time_item("start", Start),
    Start_c_str = create_time_item("start_child", Start_c),
    Stop_c_str = create_time_item("stop_child", Stop_c),
    Start_r_str = create_time_item("start_request", Start_r),
    Dur_all = create_dur_item("dur_all", Stop_c, Start),
    Dur_child = create_dur_item("dur_child", Stop_c, Start_c),
    Dur_req = create_dur_item("dur_request", Stop_r, Start_r),
    Head = msg_head(),
    %Title = make_title(Res, Id),
    Msg_title = "", %msg_title(Title),
    Date = msg_date(),
    Body = msg_res_body(St, Res),
    Foot = msg_foot(),
    Out_item = [
        Head, Msg_title, Date,
        Start_str,
        Start_c_str,
        Stop_c_str,
        Start_r_str,
        Dur_all,
        Dur_child,
        Dur_req,
        Path_str,
        Body, Foot
    ],
    mpln_p_debug:pr({?MODULE, 'make_one_jst_rss item', ?LINE, Id, Out_item},
        St#ejm.debug, run, 5),
    Out_item.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a html row from a data item of type {reference(), #jst{}}.
%% Input durations are in microseconds, output durations are in milliseconds
%%
-spec make_one_jst_html({reference(), #jst{}}) -> string().

make_one_jst_html({Id, #jst{job=J, status=St,
        t_start_child=Start_c, t_stop_child=Stop_c,
        t_start_req=Start_r, t_stop_req=Stop_r,
        start=Start, result=Res}}) ->
    J_str = make_short_info(J),
    Start_str = create_time(Start),
    Start_c_str = create_time(Start_c),
    Stop_c_str = create_time(Stop_c),
    Start_r_str = create_time(Start_r),
    %Stop_r_str = create_time(Stop_r),
    Dur_all = create_dur(Stop_c, Start),
    Dur_child = create_dur(Stop_c, Start_c),
    Dur_req = create_dur(Stop_r, Start_r),
    io_lib:format(
        "<tr>~n"
        "<td>~p</td>~n" % result
        "<td>~p</td>~n" % id
        "<td>~.3f</td>~n" % dur all
        "<td>~.3f</td>~n" % dur child
        "<td>~.3f</td>~n" % dur req
        "<td>~p</td>~n" % status
        "<td>~s</td>~n" % job receive time
        %"<td>~s</td>~n" % fetch from queue time
        "<td>~s</td>~n" % start child time
        "<td>~s</td>~n" % start http request time
        "<td>~s</td>~n" % stop child time
        "<td><pre>~s</pre></td>~n" % job info
        "</tr>~n",
        %[Res, Id, Start_str, St, Dall/1000.0, Dreq/1000.0, J_str]).
        [Res, Id, Dur_all/1000.0, Dur_child/1000.0, Dur_req/1000.0, St,
            Start_str,
            Start_c_str,
            Start_r_str,
            Stop_c_str,
            J_str]).

%%-----------------------------------------------------------------------------
create_dur({_, _, _} = T2, {_, _, _} = T1) ->
    timer:now_diff(T2, T1);
create_dur(_, _) ->
    0.

%%-----------------------------------------------------------------------------
create_time({_, _, _} = X) ->
    mpln_misc_time:get_time_str_us(X);
create_time(_) ->
    "".

%%-----------------------------------------------------------------------------
%%
%% @doc creates a string from a data item of type {reference(), #jst{}}.
%% Input durations are in microseconds, output durations are in milliseconds
%%
-spec make_one_jst_text({reference(), #jst{}}) -> string().

make_one_jst_text({Id, #jst{job=J, status=St, dur_all=Dall, dur_req=Dreq,
        start=Start, time=T}}) ->
    J_str = make_short_info(J),
    Start_str = mpln_misc_time:get_time_str_us(Start),
    Time_str = mpln_misc_time:get_time_str_us(T),
    io_lib:format("~p, ~s, ~s, ~p, ~.3f, ~.3f, ~s~n",
        [Id, Start_str, Time_str, St, Dall/1000.0, Dreq/1000.0, J_str]).

%%-----------------------------------------------------------------------------
%%
%% @doc creates output text (or html) with last jobs
%%
make_job_output("text", List) ->
    make_job_text(List);
make_job_output(_, List) ->
    make_job_html(List).

%%-----------------------------------------------------------------------------
