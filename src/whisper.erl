-module (whisper).

-behaviour(application).

%% API callbacks
-export([encrypt/1, decrypt/1, receive_function/1]).
%% Application callbacks
-export([start/2, stop/1, init/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
receive_function(From) ->
	receive
		{data, Socket, Msg} ->
			case Msg of
				{echo, Mess} ->
					From ! {bounce, Socket, Mess},
					receive_function(From);
				{keyreq} ->
					PubKey = whisper_server:get_pub_key(), Salt = whisper_server:get_salt(),
					io:format("Requested pub key and salt from ~p~n", [Socket]),
					% gen_tcp:send(Socket, converse_packet:encode({keyset, PubKey, Salt})),
					From ! {bounce, Socket, {keyset, PubKey, Salt}},
					receive_function(From);
				{keyset,K,S} ->
					io:format("Update pub key and salt~n"),
					whisper_server:change_pub_key(K), whisper_server:change_salt(S),
					receive_function(From);
				{data, Data} ->
					Receiver = get_receiver(),
					Unencrypted = decrypt(Data),
					Receiver ! {data, Socket, {data, Unencrypted}},
					receive_function(From)
			end;
		Anything ->
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