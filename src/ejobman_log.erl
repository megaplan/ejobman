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
%%% @doc job logging functions
%%% 

-module(ejobman_log).
-compile(export_all). % @todo for debug only
-export([log_job/2, log_job_result/3]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("ejobman.hrl").
-include("job.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
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
            log_job_result_2(St, R, Id);
        true ->
            ok
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc continues with writing rss message with result
%%
log_job_result_2(#ejm{jlog=Fd} = St, R, Id) ->
    msg_head(Fd),
    Title = make_title(R, Id),
    msg_title(Fd, Title),
    msg_date(Fd),
    msg_res_body(St, R),
    msg_foot(Fd).

%%-----------------------------------------------------------------------------
%%
%% @doc continues with writing rss message
%%
log_job_2(#ejm{jlog=Fd} = St, J) ->
    msg_head(Fd),
    Title = make_title(J#job.id),
    msg_title(Fd, Title),
    msg_date(Fd),
    msg_body(St, J),
    msg_foot(Fd).

%%-----------------------------------------------------------------------------
%%
%% @doc writes rss message header to file descriptor
%%
msg_head(Fd) ->
    Str = %"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        %"<rss version=\"2.0\">\n"
        %"<channel>\n"
        "<item>\n",
    write_string(Fd, Str).

%%-----------------------------------------------------------------------------
%%
%% @doc writes rss message footer to file descriptor
%%
msg_foot(Fd) ->
    Str = "</item>\n"
        %"</channel>\n"
        %"</rss>\n\n"
        ,
    write_string(Fd, Str).

%%-----------------------------------------------------------------------------
%%
%% @doc writes string/binary to file descriptor
%%
write_string(Fd, Data) when is_binary(Data) ->
    % @todo are the error logging/handling necessary here?
    catch file:write(Fd, Data);
write_string(Fd, Str) ->
    Bin = unicode:characters_to_binary(Str),
    % @todo are the error logging/handling necessary here?
    catch file:write(Fd, Bin).

%%-----------------------------------------------------------------------------
%%
%% @doc writes rss message title to file descriptor
%%
msg_title(Fd, Title) ->
    Beg = "<title><![CDATA[",
    write_string(Fd, Beg),
    write_string(Fd, Title),
    End = "]]></title>\n",
    write_string(Fd, End).

%%-----------------------------------------------------------------------------
%%
%% @doc writes rss message date to file descriptor
%%
msg_date(Fd) ->
    Lt = erlang:localtime(),
    Ts = httpd_util:rfc1123_date(Lt),
    Beg = "<pubDate>",
    write_string(Fd, Beg),
    write_string(Fd, Ts),
    End = "</pubDate>\n",
    write_string(Fd, End).

%%-----------------------------------------------------------------------------
%%
%% @doc writes rss message description to file descriptor
%%
msg_body(#ejm{jlog=Fd} = St, J) ->
    Beg = "<description><![CDATA[",
    write_string(Fd, Beg),
    Body = make_msg_body(St, J),
    write_string(Fd, Body),
    End = "]]></description>\n",
    write_string(Fd, End).

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
    io_lib:format("id=~p~ntype=~p~nmethod=~p~nurl=~p~nhost=~p~n", [
            J#job.id,
            J#job.type,
            J#job.method,
            J#job.url,
            J#job.host
        ]).

%%-----------------------------------------------------------------------------
%%
%% @doc writes rss message description with job result body to file descriptor
%%
-spec msg_res_body(#ejm{}, tuple()) -> ok.

msg_res_body(#ejm{jlog=Fd} = St, R) ->
    Beg = "<description><![CDATA[",
    write_string(Fd, Beg),
    Body_res = 
        case R of
            {ok, {_, _, _}=Result} ->
                make_msg_result_body(St, Result);
            {_, _, _} ->
                make_msg_result_body(St, R);
            {error, Reason} ->
                make_msg_result_body(St, {error, [], Reason});
            {Code, Body} ->
                make_msg_result_body(St, {Code, [], Body})
        end,
    case Body_res of
        {Beg_str, Body_str, Body_bin} ->
            write_string(Fd, Beg_str),
            write_string(Fd, "\nbody_bin="),
            write_string(Fd, Body_bin),
            write_string(Fd, "\n"),
            write_string(Fd, Body_str),
            write_string(Fd, "\n");
        _ ->
            write_string(Fd, Body_res)
    end,
    End = "]]></description>\n",
    write_string(Fd, End).

%%-----------------------------------------------------------------------------
make_title(Id) ->
    make_title({ok, {request, "", ""}}, Id).

make_title({ok, {Scode, _Body}}, Id) ->
    io_lib:format("Job ~p - ok, ~p", [Id, Scode]);
make_title({ok, {Stline, _Hdr, _Body}}, Id) ->
    io_lib:format("Job ~p - ~p", [Id, Stline]);
make_title({error, Reason}, Id) ->
    io_lib:format("Job ~p - error, ~p", [Id, Reason]).

%%-----------------------------------------------------------------------------
