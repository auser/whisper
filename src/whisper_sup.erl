-module (whisper_sup).
-behaviour(supervisor).

%% API and supervisor callbacks
-export([start_in_shell_for_testing/0, start_link/0, init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_in_shell_for_testing() ->
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
	unlink(Pid).

start_link() ->
	spawn(fun() ->
			supervisor:start_link({local, ?MODULE}, ?MODULE, _Args = [])
		end).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxTimeBetRestarts = 3600,
	TimeoutTime = 5000,
	
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
	
    WhisperServer = {whisper_server, 
		{whisper_server, start_link, []}, 
		permanent, 
		TimeoutTime, 
		worker, 
		[whisper_server]},

    LoadServers = [WhisperServer],

	{ok, {SupFlags, LoadServers}}.

%%====================================================================
%% Internal functions
%%====================================================================