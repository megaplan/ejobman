{application, ejobman_app, [
    {description, "ejobman"},
    {id, "ejobman"},
    {vsn, "1.0"},
    {modules, [
        ejobman_app.erl,
        ejobman.erl,
        ejobman_sup.erl
    ]},
    {registered, []},
    {env, []},
    {mod, {ejobman_app,[]}},
    {applications, [kernel, stdlib]}
]}.

