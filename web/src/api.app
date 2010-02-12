{application, api,
 [{description, "api"},
  {vsn, "0.1"},
  {modules, [
    api,
    api_app,
    api_sup,
    api_deps,
    api_resource
  ]},
  {registered, []},
  {mod, {api_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
