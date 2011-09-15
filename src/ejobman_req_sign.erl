%%%
%%% mpln_sign: digital signature for request
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
%%% @since 2011-09-13 16:58
%%% @license MIT
%%% @doc functions related to digital signatures for request
%%%

-module(ejobman_req_sign).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([make_sign/3]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("sign_req.hrl").
-include("types.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc creates digital signature (hmac, actually) for request using
%% base64 string encoded secret key. Returns http auth header and timestamp
%% @since 2011-09-13 18:59
%%

-spec make_sign(#req{}, string(), string()) -> {string(), string()}.

make_sign(Req, Auth_key, Secret_key) ->
    Lt = erlang:localtime(),
    Ts = httpd_util:rfc1123_date(Lt),
    make_sign(Req, Auth_key, Secret_key, Ts).
    

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc creates digital signature (hmac, actually) for request using
%% base64 string encoded secret key. Returns http auth header and timestamp
%% @since 2011-09-13 18:59
%%

-spec make_sign(#req{}, string(), string(), string()) -> {string(), string()}.

make_sign(Req, Auth_key, Secret_key, Time) ->
    Bstr = str_to_sign(Req#req{date = Time}),
    Bin_key = base64:decode(Secret_key),
    Hmac = crypto:sha_mac(Bin_key, Bstr),
    Hmac_lst = binary_to_list(Hmac),
    Hmac_txt = iolist_to_binary(
        [io_lib:format("~2.16.0b", [X]) || X <- Hmac_lst ]),
    Hmac_b64 = base64:encode_to_string(Hmac_txt),
    A_key_str = base64:decode_to_string(Auth_key),
    Hdr = A_key_str ++ ":" ++ Hmac_b64,
    {Hdr, Time}.

%%-----------------------------------------------------------------------------
%%
%% @doc joins req's fields to one string
%% @since 2011-09-13 18:59
%%
-spec str_to_sign(#req{}) -> binary().

str_to_sign(R) ->
    Lst = [
        R#req.method, % uc
        R#req.content_md5,
        R#req.ctype,
        R#req.date,
        R#req.host ++ R#req.uri
    ],
    Str = string:join(Lst, "\n"),
    unicode:characters_to_binary(Str).

%%%----------------------------------------------------------------------------
%%% Props
%%%----------------------------------------------------------------------------
-ifdef(PROPER).

-type t_sreq() :: {req, string(), string(), string(), string(), string(), string()}.

str_cut(Str) ->
    % unicode:characters_to_binary crashes on weird codes
    [X || X <- Str, (X < 55296) or (X >= 63744), X =< 65533].

req_cut(R) ->
    R#req{
        method = str_cut(R#req.method),
        content_md5 = str_cut(R#req.content_md5),
        ctype = str_cut(R#req.ctype),
        date = str_cut(R#req.date),
        host = str_cut(R#req.host),
        uri = str_cut(R#req.uri)
    }.

prop_str_sign() ->
    ?FORALL({R},
        {t_sreq()},
        begin
            Dat = req_cut(R),
            S = str_to_sign(Dat),
            is_binary(S)
        end
    ).

% I don't like this
date_cut({{Y, M, D}, T}) when Y < 1970 ->
    date_cut({{1971, M, D}, T});
date_cut({{1970, 1, 1}, {H, Mn, S}}) when H =< 12 ->
    {{1970, 1, 1}, {13, Mn, S}};
date_cut({{Y, 2, D}, T}) when D >= 29 ->
    {{Y, 3, D}, T};
date_cut({{Y, 4, D}, T}) when D >= 31 ->
    {{Y, 5, D}, T};
date_cut({{Y, 6, D}, T}) when D >= 31 ->
    {{Y, 7, D}, T};
date_cut({{Y, 9, D}, T}) when D >= 31 ->
    {{Y, 8, D}, T};
date_cut({{Y, 11, D}, T}) when D >= 31 ->
    {{Y, 12, D}, T};
date_cut(D) ->
    D.

prop_make_sign() ->
    ?FORALL({R, Ka, Ks, T},
        {t_sreq(), binary(), binary(), string()},
        begin
            crypto:start(),
            T2 = str_cut(T),
            R2 = req_cut(R),
            Ka2 = base64:encode_to_string(Ka),
            Ks2 = base64:encode_to_string(Ks),
            {Hdr, Date} = make_sign(R2, Ka2, Ks2, T2),
            ?WHENFAIL(error_logger:info_report({failed, Hdr, Date}),
                is_list(Hdr) and is_list(Date)
            )
        end
    ).

-endif.
%%-----------------------------------------------------------------------------
