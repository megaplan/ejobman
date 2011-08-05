%%%
%%% binary data test for http request
%%%
-module(t5).
-compile(export_all).
-define(HTTP_TIMEOUT, 5000).

t() ->
    P = p(),
    proceed(P).

t2() ->
    P = p2(),
    proceed(P).

proceed(Req) ->
    inets:start(),
    ssl:start(),
    Res = http:request(post, Req,
        [{timeout, ?HTTP_TIMEOUT}, {connect_timeout, ?HTTP_TIMEOUT}],
        []),
    error_logger:info_report({?MODULE, t2, res, Res}),
    Res
.

p2() ->
    Hdr = [{"Host", "mdt.megaplan.user1"}],
    Ctype = "application/x-www-form-urlencoded",
    Pars = [{<<"test">>,<<"test1 test 2 tesdfa">>}],
    Body = mochiweb_util:urlencode(Pars),
    Url = "http://192.168.1.1/admin/pre",
    Req = {Url, Hdr, Ctype, Body},
    error_logger:info_report({?MODULE, t2, req, Req}),
    Req.

p() ->
    Hdr = [{"Host", <<"host3.localdomain">>}],
    Ctype = "application/x-www-form-urlencoded",
    Pars = [{<<"test">>,<<"test1 test 2 tesdfa">>}],
    Body = mochiweb_util:urlencode(Pars),
    Url = "http://localhost:8182/?page",
    Req = {Url, Hdr, Ctype, Body},
    error_logger:info_report({?MODULE, t, req, Req}),
    Req.
