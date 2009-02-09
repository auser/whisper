-module (whisper_utils).

-export ([get_app_env/2]).

% Get the environment
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.

running_receiver(undefined, Fun) ->
		run_fun(Fun);

running_receiver(Pid, Fun) when is_pid(Pid) ->
	case is_process_alive(Pid) of
		true -> Pid;
		false ->run_fun(Fun)
	end.

run_fun(Fun) ->
	case length(Fun) of
		2 -> [M,F] = Fun;
		1 -> [M] = Fun, F = receive_function
	end,
	A = [self()],
	proc_lib:spawn_link(M,F,A).