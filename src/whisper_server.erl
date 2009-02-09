-module (whisper_server).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, encrypt/1, decrypt/1, get_receiver/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
			pub_key,
			priv_key,
			n,
			receiver, % layers
			receive_function,
			type % rsa,tls, etc.
			}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
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

encrypt(Msg) -> gen_server:call(?SERVER, {encrypt, Msg}).
decrypt(Msg) -> gen_server:call(?SERVER, {decrypt, Msg}).

get_receiver() ->
	gen_server:call(?SERVER, {get_receiver}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	start_link(rsa).
	
start_link(Config) ->
	io:format("In init for whisper.erl~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Config]) ->	
	application:start(crypto),
	Type = whisper_utils:get_app_env(type, rsa),
	Fun = config:parse(successor, Config),
	{Pub,Priv,N} = Type:init(),
  {ok, #state{priv_key = Priv, pub_key = Pub, n = N, type = Type, receiver=undefined, receive_function=Fun}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({encrypt, Data}, _From, #state{n = N, pub_key = Pub, type = Type} = State) ->
	Reply = Type:encrypt({N, Pub}, Data),
	{reply, Reply, State};

handle_call({decrypt, Data}, _From, #state{n = N, priv_key = Priv, type = Type} = State) ->
	Reply = Type:decrypt({N, Priv}, Data),
	{reply, Reply, State};

handle_call({get_receiver}, _From, #state{receive_function = RecFun, receiver = Pid} = State) ->	
	ReceiverPid = whisper_utils:running_receiver(Pid, RecFun),
	{reply, ReceiverPid, State#state{receiver = ReceiverPid}};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------