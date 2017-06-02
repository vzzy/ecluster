%% @author bai
%% @doc @todo Add description to ecluster.


-module(ecluster).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	send/2,
		 
	start/0,
	stop/0			 
]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% 启动方法
start()->
	ok = start(?MODULE),
	ok.

%% 关闭方法
stop()->
	ecluster_app:stop(normal),
	application:stop(?MODULE),
	timer:sleep(5000),
	erlang:halt(),
	ok.	


%% 启动App
start(App) ->
    start_ok(App, application:start(App, permanent)).
start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

%% 发消息方法
send(Pid,Packet)when is_pid(Pid)->
	Pid ! {send,Packet};
send(Socket,Packet)->
	gen_tcp:send(Socket,Packet).

