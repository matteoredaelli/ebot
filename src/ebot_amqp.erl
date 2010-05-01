%%%-------------------------------------------------------------------
%%% File    : ebot_amqp.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created : 23 Apr 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_amqp).

-author("matteo.redaelli@@libero.it").

-define(SERVER, ?MODULE).
-define(EBOT_EXCHANGE, <<"EBOT">>).
-define(EBOT_QUEUE_URL_NEW, <<"EBOT_QUEUE_URL_NEW">>).
-define(EBOT_KEY_URL_NEW, <<"ebot.url.new">>).
-define(EBOT_KEY_URL_PROCESSED, <<"ebot.url.processed">>).
-define(EBOT_KEY_URL_REFUSED, <<"ebot.url.refused">>).
-define(TIMEOUT, 10000).

-behaviour(gen_server).

-include("../deps/rabbitmq-erlang-client/include/amqp_client.hrl").


%% API
-export([
	 start_link/0,
	 add_new_url/1,
	 add_refused_url/1,
	 get_new_url/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  channel,
	  config,
	  connection,
	  exchange = ?EBOT_EXCHANGE
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_new_url(Url) ->
    gen_server:cast(?MODULE, {add_new_url, Url}).

add_refused_url(Url) ->
    gen_server:cast(?MODULE, {add_refused_url, Url}).

get_new_url(Depth) ->
    gen_server:call(?MODULE, {get_new_url, Depth}).

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
init([]) ->
   case ebot_util:load_settings(?MODULE) of
	{ok, Config} ->
	   case ampq_connect_and_get_channel() of
	       {ok, {Connection, Channel}} ->
		   TotQueues = proplists:get_value(tot_new_urls_queues, Config),
		   amqp_setup_new_url_consumers(
		     Channel, TotQueues),
		   {ok, #state{
		      channel = Channel,
		      connection = Connection,
		      config=Config}
		   };
	       _Else ->
		   {error, amqp_cannot_connect_or_get_channel}
	   end;
	_Else ->
	    {error, cannot_load_configuration}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({get_new_url, Depth}, _From, State) ->
    Channel =  State#state.channel,
    Reply = amqp_basic_get_message(Channel, get_new_queue_name(Depth)),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_new_url, Url}, State) ->
    Depth = ebot_url_util:url_depth(Url),
    Key = get_new_queue_name(Depth),
    amqp_send_message(Key, Url, State),
    {noreply, State};

handle_cast({add_refused_url, Url}, State) ->
    amqp_send_message(?EBOT_KEY_URL_REFUSED, Url, State),
    {noreply, State};

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
terminate(_Reason, State) ->
    Connection = State#state.connection,
    Channel =  State#state.channel,
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
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

get_config(Option, State) ->
    proplists:get_value(Option, State#state.config).

ampq_connect_and_get_channel() ->
    %% Start a connection to the server
    Connection = amqp_connection:start_network(),

    %% Once you have a connection to the server, you can start an AMQP channel
    %% TODO : verify 

    Channel = amqp_connection:open_channel(Connection),
    ExchangeDeclare = #'exchange.declare'{
      exchange = ?EBOT_EXCHANGE, 
      type = <<"topic">>
      %% uncomment the following row if you want a durable exchange
      %% , durable = true
     },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),
    {ok, {Connection,Channel}}.

amqp_basic_get_message(Channel, Queue) ->
     case amqp_channel:call(Channel, 
			    #'basic.get'{queue = Queue, no_ack = true}) of
	 {#'basic.get_ok'{}, Content} ->
	     #amqp_msg{payload = Payload} = Content,
	     io:format("Payload received: queue=~p, val=~p~n", [Queue, Payload]),
	     Payload;
	 _Else ->
	     empty
     end.

amqp_send_message(RoutingKey, Payload, State) ->
    Channel =  State#state.channel,
    Exchange =  State#state.exchange,
    BasicPublish = #'basic.publish'{exchange = Exchange, 
				    routing_key = RoutingKey},

    Msg = #amqp_msg{
      payload = Payload
      %% uncomment the following row if you want a durable message
      %%, props = #'P_basic'{delivery_mode=2}
     },
    case Result = amqp_channel:cast(Channel, BasicPublish, _MsgPayload = Msg) of
	ok ->
	    io:format("amqp_send_message: ok: Key=~p, Payload=~p~n", 
		      [RoutingKey,Payload]);
	else ->
	    io:format("amqp_send_message: failed: Key=~p, Payload=~p~n", 
		      [RoutingKey,Payload])
    end,
    Result.
 
amqp_setup_new_url_consumers(Channel, Tot) ->
    lists:foreach(
      fun(N) ->
	      KeyQueue =  get_new_queue_name(N),
	      amqp_setup_consumer(
		  Channel,
		  KeyQueue,
		  ?EBOT_EXCHANGE,
		  KeyQueue
		 ) end,
      lists:seq(0, Tot)
     ).

amqp_setup_consumer(Channel, Q, X, Key) ->
    QueueDeclare = #'queue.declare'{queue=Q
				    %%, durable=true
				   },
    #'queue.declare_ok'{queue = Q,
                        message_count = MessageCount,
                        consumer_count = ConsumerCount}
	= amqp_channel:call(Channel, QueueDeclare),
    
    log(queue,Q),
    log(message_count,MessageCount),
    log(consumer_count,ConsumerCount),

    QueueBind = #'queue.bind'{queue = Q,
                              exchange = X,
                              routing_key = Key},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind).

log(Key,Value) ->
    io:format("~p: ~p~n",[Key,Value]).

get_new_queue_name(Depth) ->
    list_to_binary( 
      binary_to_list(?EBOT_KEY_URL_NEW) ++ 
      "." ++ 
      integer_to_list(Depth)).
