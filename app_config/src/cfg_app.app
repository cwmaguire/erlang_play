{application, cfg,
 [{description, "App configuration test"},
  {vsn, "1"},
  {modules, [cfg_app, cfg_sup, cfg]},
  {registered, [cfg]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {cfg_app,[]}},
  {env, [{a, 1}, {b, 0}, {c, 4}]}
 ]}.
