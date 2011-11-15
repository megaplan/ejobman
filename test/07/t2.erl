% test configured groups

-module(t2).
-compile(export_all).

-include("../../include/job.hrl").

r() ->
    r(20).

r(Max) ->
    F = fun(X) -> spawn(?MODULE, r2, [X]) end,
    lists:map(F, lists:seq(1, Max)).

r2(_X) ->
    Job = t(),
    Res = ejobman_handler:cmd(Job),
    error_logger:info_report({?MODULE, r2_res, Job, Res}).

t() ->
    S = s(),
    D = make_job(S),
    D
.

make_group(_Info) ->
    <<"group_1">>.

make_job(Data) ->
    crypto:start(),
    Info = ejobman_data:get_rest_info(Data),
    Method = ejobman_data:get_method(Info),
    Url_base = ejobman_data:get_url(Info),
    R = crypto:rand_uniform(0, 10000),
    Url_n = list_to_binary(integer_to_list(R)),
    Url = <<Url_base/binary, Url_n/binary>>,
    Host = ejobman_data:get_host(Info),
    Params = ejobman_data:get_params(Info),
    Group = make_group(Info),
    #job{
        group = Group,
        method = Method,
        url = Url,
        host = Host,
        params = Params
    }.

s() ->
{struct,
    [{<<"type">>,<<"rest">>},
     {<<"info">>,
      {struct,
          [{<<"group">>,<<"main_group">>},
          {<<"method">>,<<"get">>},
           {<<"url">>,
            <<"http://localhost:8184/test_group_limit.yaws?new_id=">>},
           {<<"params">>,
            {struct,
                [{<<"orderId">>,123134},
                 {<<"data">>,
                  {struct,
                      [{<<"name">>,<<"Yourname">>},
                       {<<"time">>,
                        <<"Fri, 12 Oct 2011 17:25:54 +0400">>
                       }
                      ]
                  }
                 }
                ]
            }
           }
          ]
      }
     }
    ]
}
.

s2() ->
    <<"{\"type\":\"rest\",\"info\":{\"method\":\"post\",\"url\":\"http:\\/\\/bfg.megaplan.kulikov\\/BumsTask\\/AmqpTest\\/myJobAction\",\"params\":{\"orderId\":123134,\"data\":{\"name\":\"Dima\",\"time\":\"Fri, 14 Oct 2011 17:25:54 +0400\"}}}}">>
.

s3() ->
{<<"params">>,
 {struct,
     [{<<"orderId">>,123134},
      {<<"data">>,
       {struct,
           [{<<"name">>,<<"Dima">>},
            {<<"time">>,
             <<"Fri, 14 Oct 2011 17:25:54 +0400">>
            }
           ]
       }
      }
     ]
 }
}
.
