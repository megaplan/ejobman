{application, ejobman_app, [
    {description, "ejobman"},
    {id, "ejobman"},
    {vsn, "1.0"},
    {modules, [
        ejobman_app.erl,
        ejobman_child.erl,
        ejobman_child_sup.erl,
        ejobman_conf.erl,
        ejobman_conf_rabbit.erl,
        ejobman_handler_cmd.erl,
        ejobman_handler.erl,
        ejobman_rb.erl,
        ejobman_receiver_cmd.erl,
        ejobman_receiver.erl,
        ejobman_sup.erl,
        mpln_misc_conf.erl,
        mpln_misc_json.erl,
        mpln_misc_log.erl,
        misc_time.erl,
        p_debug.erl
    ]},
    {registered, []},
    {env, []},
    {mod, {ejobman_app,[]}},
    {applications, [kernel, stdlib]}
]}.

