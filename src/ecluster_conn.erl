%% @author bai
%% @doc @todo Add description to ecluster_conn.


-module(ecluster_conn).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start/1,
	start/2,
	
	stop/1		 
]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
%% 与节点保持连接间隔
-define(KEEPALIVE_CONN_INTVL,3).
-record(state, {
	nodes = [],
	conn_intvl = ?KEEPALIVE_CONN_INTVL				
}).


%% 启动 {ok,Pid} | ignore | {error,Error}
start(Nodes) ->
	start(Nodes,?KEEPALIVE_CONN_INTVL).
start(Nodes,Conn_intvl) ->
    gen_server:start({local,?MODULE},?MODULE, [Nodes,Conn_intvl], []).

%% 终止进程
stop(Pid) when is_pid(Pid)->
    Pid ! stop;
stop(_Pid)->
	ok.

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Nodes,Conn_intvl]) ->
	_New_Timer_ref = erlang:start_timer(0, self(), keepalive_conn),
    {ok, #state{
		nodes = Nodes,
		conn_intvl = Conn_intvl			
	}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
%% 空闲超时，重连节点
handle_info({timeout, _Timer_ref, keepalive_conn}, #state{nodes = Nodes,conn_intvl = Conn_intvl} = State) ->
	_New_Timer_ref = erlang:start_timer(Conn_intvl, self(), keepalive_conn),
	My_Nodes = [node()] ++ nodes(),
	catch conn_nodes(Nodes,My_Nodes),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% 重连节点，奖励集群
conn_nodes([],_My_Nodes)->
	ok;
conn_nodes([Node,T],My_Nodes)->
	case lists:member(Node, My_Nodes) of
		false-> %% 不在集群内，重连种子节点  ignored  | false | true
			catch net_kernel:connect_node(Node);
		true-> %% 已在集群内
			ok
	end,
	conn_nodes(T,My_Nodes).

%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


