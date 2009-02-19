-module (whisper_server).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, encrypt/1, decrypt/1, get_receiver/0]).
-export ([change_pub_key/1, change_priv_key/1, change_salt/1]).
-export ([get_pub_key/0, get_priv_key/0, get_salt/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
			pub_key,
			priv_key,
			n,
			successor_mfa, % layers
			type % rsa,tls, etc.
			}).
-define(SERVER, ?MODULE).
-define(APPLICATIONS_TO_START, [crypto]).

%%====================================================================
%% API
%%====================================================================
change_pub_key(Key) -> gen_server:cast(?SERVER, {pub_key_change, Key}).
get_pub_key() -> gen_server:call(?SERVER, {get_pub_key}).
change_priv_key(Key) -> gen_server:cast(?SERVER, {priv_key_change, Key}).
get_priv_key() -> gen_server:call(?SERVER, {get_priv_key}).
change_salt(Salt) -> gen_server:cast(?SERVER, {salt_change, Salt}).
get_salt() -> gen_server:call(?SERVER, {get_salt}).

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
	NewConfig = lists:append([{enc_type, rsa}], Config),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [NewConfig], []).

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
	Type = config:parse(enc_type, Config),
	Fun = case config:parse(successor, Config) of
		{} -> [?MODULE];
		A -> A
	end,
	{Pub,Priv,N} = Type:init(),
	io:format("Whisper fun: ~p~n", [Fun]),	
	layers:register_process(Fun, self()),
  {ok, #state{priv_key = Priv, pub_key = Pub, n = N, type = Type, successor_mfa=Fun}}.

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

handle_call({get_receiver}, _From, #state{successor_mfa = RecFun} = State) ->	
	{reply, RecFun, State};

handle_call({get_pub_key}, _From, #state{pub_key=Pub} = State) -> {reply, Pub, State};
handle_call({get_priv_key}, _From, #state{priv_key=Priv} = State) -> {reply, Priv, State};
handle_call({get_salt}, _From, #state{n=Salt} = State) -> {reply, Salt, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({pub_key_change, Key}, State) -> {noreply, State#state{pub_key = Key}};
handle_cast({priv_key_change, Key}, State) -> {noreply, State#state{priv_key = Key}};
handle_cast({salt_change, Salt}, State) -> {noreply, State#state{n = Salt}};
	
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	io:format("Received ~p in ~p~n", [Info, ?MODULE]),
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