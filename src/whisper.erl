-module (whisper).

-behaviour(application).

%% API callbacks
-export([encrypt/1, decrypt/1]).
%% Application callbacks
-export([start/2, stop/1, init/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
receive_function(From) ->
	receive
		{data, Socket, Data} ->
			Receiver = get_receiver(),
			Unencrypted = decrypt(Data),
			io:format("Sending unencrypted ~p~n", [Unencrypted]),
			Receiver ! {data, Socket, Unencrypted};
		Anything ->
			io:format("Received ~p~n", [Anything]),
			receive_function(From)
	end.

encrypt(Msg) -> whisper_server:encrypt(Msg).
decrypt(Msg) -> whisper_server:decrypt(Msg).
get_receiver() -> whisper_server:get_receiver().

%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, Config) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_State) ->
  ok.


init([Config]) -> 
	application:start(crypto),
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxTimeBetRestarts = 3600,
	TimeoutTime = 5000,

	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

	WhisperServer = 
	{whisper,
		{whisper_server, start_link, [Config]}, 
		permanent, 
		TimeoutTime, 
		worker, []
	},

	LoadServers = [WhisperServer],

	{ok, {SupFlags, LoadServers}}.

%%====================================================================
%% Internal functions
%%====================================================================