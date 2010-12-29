%% EBOT, an erlang web crawler.
%% Copyright (C) 2010 ~ matteo DOT redaelli AT libero DOT it
%%                      http://www.redaelli.org/matteo/
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%%%-------------------------------------------------------------------
%%% File    : ebot_mq.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created : 23 Apr 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_mq).

-author("matteo.redaelli@@libero.it").

-define(SERVER, ?MODULE).
-define(EBOT_EXCHANGE, <<"EBOT">>).
-define(EBOT_URL_KEYS, [
		    <<"new">>, 
		    <<"fetched">>, 
		    <<"processed">>, 
		    <<"refused">>
		   ]).
-define(EBOT_URL_KEY_PREFIX, <<"ebot.url.">>).
-define(TIMEOUT, 10000).

-behaviour(gen_server).

-include("deps/rabbitmq-erlang-client/include/amqp_client.hrl").


%% API
-export([
	 start_link/0,
	 receive_url_fetched/1,
	 receive_url_new/1,
	 send_url_fetched/1,
	 send_url_new/1,
	 send_url_processed/1,
	 send_url_refused/1,
	 statistics/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  channel,
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
receive_url_fetched(Depth) ->
    gen_server:call(?MODULE, {receive_url, <<"fetched">>, Depth}).
receive_url_new(Depth) ->
    gen_server:call(?MODULE, {receive_url, <<"new">>, Depth}).
send_url_fetched(Payload) ->
    gen_server:cast(?MODULE, {send_url, <<"fetched">>, Payload}).
send_url_new(Payload) ->
    gen_server:cast(?MODULE, {send_url, <<"new">>, Payload}).
send_url_processed(Payload) ->
    gen_server:cast(?MODULE, {send_url, <<"processed">>, Payload}).
send_url_refused(Payload) ->
    gen_server:cast(?MODULE, {send_url, <<"refused">>, Payload}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).

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
    {ok, Durable} = ebot_util:get_env(mq_durable_queues),
    {ok, Params} = ebot_util:get_env(mq_connection_params),
    AMQParams = #amqp_params{
      username =  proplists:get_value(username, Params),
      password =  proplists:get_value(password, Params),
      host =  proplists:get_value(host, Params),
      virtual_host =  proplists:get_value(virtual_host, Params),
      channel_max =  proplists:get_value(channel_max, Params)
     },
    case ampq_connect_and_get_channel(AMQParams, Durable) of
	{ok, {Connection, Channel}} ->
	    {ok, TotQueues} = ebot_util:get_env(mq_priority_url_queues),
	    lists:foreach(
	      fun(Key) ->
		      amqp_setup_url_consumers(Channel, Key, TotQueues, Durable)
	      end,
	      ?EBOT_URL_KEYS),
	    {ok, #state{
	       channel = Channel,
	       connection = Connection
	      }
	    };
	_Else ->
	    {error, amqp_cannot_connect_or_get_channel}
	    %% TODO stopping the application or wait 60 sec and run again init
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

handle_call({receive_url, Key, Depth}, _From, State) ->
    Reply = amqp_receive_url(Key, Depth, State),
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    Channel =  State#state.channel,
    {ok, TotQueues} = ebot_util:get_env(mq_priority_url_queues),
    Reply = lists:foldl(
	       fun(Key, Result) ->
		       Result ++ statistics_by_key(Channel, Key, TotQueues)
	       end,
	      [],
	      ?EBOT_URL_KEYS),
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
handle_cast({send_url, Key, Payload}, State) ->
    amqp_send_url(Key, Payload, State),
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

ampq_connect_and_get_channel(Params, Durable) ->
    %% Start a connection to the server
    {ok, Connection} = amqp_connection:start(network, Params),

    %% Once you have a connection to the server, you can start an AMQP channel
    %% TODO : verify 

    {ok, Channel} = amqp_connection:open_channel(Connection),
    ExchangeDeclare = #'exchange.declare'{
      exchange = ?EBOT_EXCHANGE, 
      type = <<"topic">>,
      durable = Durable
     },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),
    {ok, {Connection,Channel}}.

amqp_basic_get_message(Channel, Queue) ->
     case amqp_channel:call(Channel, 
			    #'basic.get'{queue = Queue, no_ack = true}) of
	 {#'basic.get_ok'{}, Content} ->
	     #amqp_msg{payload = Payload} = Content,
%	     {Url, _} = Payload,
	     error_logger:info_report({?MODULE, ?LINE, {get_url, from_queue, Queue}}),
	     {ok, payload_decode(Payload)};
	 Else ->
	     {error, Else}
     end.

amqp_receive_url(Key, Depth, State) ->
    Channel =  State#state.channel,
    Queue = get_url_queue_name(Key, Depth),
    amqp_basic_get_message(Channel, Queue).

amqp_send_url(Key, Payload = {Url, _}, State) ->
    {ok, {Module, Function}} = ebot_util:get_env(mq_url_priority_plugin),
    Depth = Module:Function(Url),
    RoutingKey = get_url_queue_name(Key, Depth),
    amqp_send_message(RoutingKey, Payload, State).

amqp_send_message(RoutingKey, Payload = {Url,_}, State) ->
    Channel =  State#state.channel,
    Exchange =  State#state.exchange,
    BasicPublish = #'basic.publish'{exchange = Exchange, 
				    routing_key = RoutingKey},

    NewPayload = payload_encode(Payload),
    {ok, Durable} = ebot_util:get_env(mq_durable_queues),
    case Durable of
	true ->
	    Msg = #amqp_msg{
	      payload = NewPayload,
	      props = #'P_basic'{delivery_mode=2}
	     };
	false ->
	    Msg = #amqp_msg{
	      payload = NewPayload
	     }
    end,
    case Result = amqp_channel:cast(Channel, BasicPublish, _MsgPayload = Msg) of
	ok ->
	    error_logger:info_report({?MODULE, ?LINE, {send_url, RoutingKey, Url}});
	else ->
	    error_logger:error_report({?MODULE, ?LINE, {cannot_send_url, RoutingKey, Url}})
    end,
    Result.

amqp_setup_url_consumers(Channel, Key, Tot, Durable) ->
    lists:foreach(
      fun(N) ->
	      KeyQueue = get_url_queue_name(Key, N),
	      amqp_setup_consumer(
		Channel,
		KeyQueue,
		Durable
	       ) end,
      lists:seq(0, Tot)
     ).

amqp_setup_consumer(Channel, QueueKey, Durable) ->
    amqp_setup_consumer(
      Channel,
      QueueKey,
      ?EBOT_EXCHANGE,
      QueueKey,
      Durable
     ).

amqp_setup_consumer(Channel, Q, X, Key, Durable) ->
    QueueDeclare = #'queue.declare'{queue=Q, durable=Durable},
    #'queue.declare_ok'{queue = Q,
                        message_count = MessageCount,
                        consumer_count = ConsumerCount}
	= amqp_channel:call(Channel, QueueDeclare),
    
    log(queue,{Q, {message_count,MessageCount}, {consumer_count,ConsumerCount}}),

    QueueBind = #'queue.bind'{queue = Q,
                              exchange = X,
                              routing_key = Key},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind).


get_url_queue_name(Key, Depth) ->
    BinDepth = list_to_binary("." ++ integer_to_list(Depth)),
    <<?EBOT_URL_KEY_PREFIX/binary,Key/binary,BinDepth/binary>>.

statistics_by_key(Channel, Key, TotQueues) ->
    lists:map(
      fun(N) -> 
	      Q = get_url_queue_name(Key, N), 
	      statistics_by_queue(Channel, Q)
      end,
      lists:seq(0, TotQueues)
     ).

statistics_by_queue(Channel, Q) ->
    QueueDeclare = #'queue.declare'{queue=Q},
    #'queue.declare_ok'{queue = Q,
                        message_count = MessageCount,
                        consumer_count = _ConsumerCount}
	= amqp_channel:call(Channel, QueueDeclare),
    {Q, MessageCount}.


log(Key,Value) ->
    error_logger:info_report({?MODULE, ?LINE, {Key,Value }}),
    io:format("~p: ~p~n",[Key,Value]).

payload_decode(Payload) ->
     binary_to_term(Payload).
payload_encode(Payload) ->
     term_to_binary(Payload, [compressed]).

%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_mq_test() ->
    Url = <<"http://www.redaelli.org/matteo/ebot_test/">>,
    Payload = {Url,false},
    ?assertEqual( Payload, 
		  payload_decode(payload_encode(Payload))),
    ?assertEqual( <<"ebot.url.new.1">>, 
		  get_url_queue_name(<<"new">>, 1)),
    ebot_mq:send_url_new(Payload),
    ?assertEqual( {ok, Payload}, 
		  ebot_mq:receive_url_new(2) ).

-endif.
