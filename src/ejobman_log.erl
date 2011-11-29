%%%
%%% ejobman_log: job logging
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
-export([log_job/2, log_job_result/3]).
-export([make_jlog_xml/1, make_jlog_xml/2]).

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

%%-----------------------------------------------------------------------------
%%
%% @doc writes rss message with job request to file descriptor
%% @since 2011-10-20 11:35
%%
-spec log_job(#ejm{}, #job{}) -> ok.

log_job(#ejm{debug=D} = St, J) ->
    N = proplists:get_value(job, D, -1),
    if  N > 0 ->
            log_job_2(St, J);
        true ->
            ok
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc writes rss message with job result to file descriptor
%% @since 2011-10-20 11:35
%%
-spec log_job_result(#ejm{}, tuple(), reference()) -> ok.

log_job_result(#ejm{debug=D} = St, R, Id) ->
    N = proplists:get_value(job, D, -1),
    if  N > 0 ->
            common_log_job_result(St, R, Id),
            log_job_result_2(St, R, Id);
        true ->
            ok
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc logs result to common erpher log
%%
common_log_job_result(St, {ok, Result}, Id) ->
    {St_head, Body_str, Bin} = make_msg_result_body(St, Result),
    mpln_p_debug:pr({?MODULE, 'common_log_job_result', ?LINE,
        Id, St_head}, St#ejm.debug, http, 3),
    mpln_p_debug:pr({?MODULE, 'common_log_job_result', ?LINE,
        Id, Body_str}, St#ejm.debug, http, 5),
    mpln_p_debug:pr({?MODULE, 'common_log_job_result', ?LINE,
        Id, Bin}, St#ejm.debug, http, 4);
common_log_job_result(St, {error, Reason}, Id) ->
    mpln_p_debug:pr({?MODULE, 'common_log_job_result error', ?LINE,
        Id, Reason}, St#ejm.debug, http, 2);
common_log_job_result(St, {Other, Reason}, Id) ->
    mpln_p_debug:pr({?MODULE, 'common_log_job_result other', ?LINE,
        Id, Other, Reason}, St#ejm.debug, http, 2)
.

%%-----------------------------------------------------------------------------

%%
%% @doc continues with writing rss message with result
%%
log_job_result_2(#ejm{jlog=Fd} = St, R, Id) ->
    T1 = msg_head(),
    Title = make_title(R, Id),
    T2 = msg_title(Title),
    T3 = msg_date(),
    T4 = msg_res_body(St, R),
    T5 = msg_foot(),
    write_string(Fd, [T1, T2, T3, T4, T5]).

%%-----------------------------------------------------------------------------
%%
%% @doc continues with writing rss message
%%
log_job_2(#ejm{jlog=Fd} = St, J) ->
    T1 = msg_head(),
    Title = make_title(J#job.id),
    T2 = msg_title(Title),
    T3 = msg_date(),
    T4 = msg_body(St, J),
    T5 = msg_foot(),
    write_string(Fd, [T1, T2, T3, T4, T5]).

%%-----------------------------------------------------------------------------
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
%% @doc writes string/binary to file descriptor
%%
write_string(Fd, Str) ->
    Bin = unicode:characters_to_binary(Str),
    % @todo are the error logging/handling necessary here?
    catch file:write(Fd, Bin).

%%-----------------------------------------------------------------------------
%%
%% @doc returns rss message title
%%
msg_title(Title) ->
    Beg = "<title><![CDATA[",
    End = "]]></title>\n",
    [Beg, Title, End].

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
%% @doc returns rss message description
%%
msg_body(St, J) ->
    Beg = "<description><![CDATA[<pre>",
    Body = make_msg_body(St, J),
    End = "</pre>]]></description>\n",
    [Beg, Body, End].

%%-----------------------------------------------------------------------------
%%
%% @doc creates rss message description in dependence of the configured
%% debug level
%%
-spec make_msg_body(#ejm{}, #job{}) -> string().

make_msg_body(#ejm{debug=D}, J) ->
    N = proplists:get_value(job, D, 0),
    if  N >= 6 -> % everything
            make_short_info(J)
            ++ io_lib:format( "auth=~p~nparams=~p~nrun_time=~p~n",
                [J#job.auth, J#job.params, J#job.run_time]);
        N >= 4 -> % no passwords/auth
            make_short_info(J)
            ++ io_lib:format("params=~p~nrun_time=~p~n",
                [J#job.params, J#job.run_time]);
        N >= 3 -> % short info
            make_short_info(J);
        N >= 2 -> % very short info
            io_lib:format("id=~p~nmethod=~p~nurl=~p~n",
                [J#job.id, J#job.method, J#job.url]);
        N > 0 ->  % id
            io_lib:format("id=~p~n", [J#job.id]);
        true ->
            ""
    end.

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
make_short_info(J) ->
    io_lib:format("id=~p~ntype=~p~ngroup=~p~n"
        "method=~p~nurl=~p~nhost=~p~n", [
            J#job.id,
            J#job.type,
            J#job.group,
            J#job.method,
            J#job.url,
            J#job.host
        ]).

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
%% @doc creates title with id for rss item
%%
make_title(Id) ->
    make_title({ok, {request, "", ""}}, Id).

make_title({ok, {Scode, _Body}}, Id) ->
    io_lib:format("Job ~p - ok, ~p", [Id, Scode]);
make_title({ok, {Stline, _Hdr, _Body}}, Id) ->
    io_lib:format("Job ~p - ~p", [Id, Stline]);
make_title({error, Reason}, Id) ->
    io_lib:format("Job ~p - error, ~p", [Id, Reason]).

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
create_item_date(I) ->
    ["<pubDate>",
    I#item.date,
    "</pubDate>\n"].

%%-----------------------------------------------------------------------------
create_item_title(I) ->
    ["<title><![CDATA[Job - ",
    I#item.ref,
    " - ",
    I#item.status,
    "]]></title>\n"].

%%-----------------------------------------------------------------------------
