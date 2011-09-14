-ifndef(sign_req).
-define(sign_req, true).

-record(req, {
    method,
    content_md5,
    ctype,
    date,
    host,
    uri
}).

-endif.
