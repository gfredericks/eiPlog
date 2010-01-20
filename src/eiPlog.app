{application, eiPlog,
 [{description, "eiPlog"},
  {vsn, "0.1"},
  {modules, [
    eiPlog,
    eiPlog_app,
    eiPlog_sup,
    eiPlog_deps,
    eiPlog_resource
  ]},
  {registered, []},
  {mod, {eiPlog_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
