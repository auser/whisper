{application, whisper,
 [
  {description, "Whisper app"},
  {vsn, "1.0"},
  {id, "whisper"},
  {modules,      [whisper, whisper_app, cryptography]},
  {registered,   [whisper, whisper_app, whisper_sup]},
  {applications, [kernel, stdlib,crypto]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {whisper_app, []}},
  {env, []}
 ]
}.