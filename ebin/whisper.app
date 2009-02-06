{application, whisper,
 [
  {description, "Whisper app"},
  {vsn, "1.0"},
  {id, "whisper"},
  {modules,      [whisper, cryptography]},
  {registered,   [whisper, cryptography]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {whisper_app, []}},
  {env, []}
 ]
}.