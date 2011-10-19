-module(t1).
-compile(export_all).

-include("../../include/job.hrl").

t() ->
    S = s(),
    D = make_job(S),
    D
.

make_job(Data) ->
    Info = ejobman_data:get_rest_info(Data),
    Method = ejobman_data:get_method(Info),
    Url = ejobman_data:get_url(Info),
    Host = ejobman_data:get_host(Info),
    Params = ejobman_data:get_params(Info),
    #job{
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
          [{<<"method">>,<<"post">>},
           {<<"url">>,
            <<"http://bfg.megaplan.kulikov/BumsTask/AmqpTest/myJobAction">>},
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
