{application, eiPlog,
 [{description, "eiPlog"},
  {vsn, "0.1"},
  {modules, [
    eiPlog,
    eiPlog_app,
    eiPlog_sup,
    eiPlog_deps,
    eiPlog_keyholder,
    eiPlog_mysql,
    eiPlog_resource_applications,
    eiPlog_resource_events,
    eiPlog_resource_logs
  ]},
  {registered, []},
  {mod, {eiPlog_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
