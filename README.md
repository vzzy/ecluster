安装   
===================================   
集群模块:配置简单。   种子节点列表、探测时间。

#0. 环境：

    otp17.5+
    GNU Make
    rebar
    
#1. 编译

   xxx.app.src
    {applications, [
  		kernel,
  		stdlib,
  		ecluster
  	]},

	%% ecluster配置
	  {ecluster,[
	    {nodes,[
	    	'ecluster1@127.0.0.1',
	    	'ecluster2@127.0.0.1'
	   	]},  							%% 种子节点列表
	    {conn_intvl,5000}  				%% 重连间隔(毫秒)
	  ]}


	# make all        
	
	# make issue
	
	启动五个节点进行校验
	./ecluster1
	./ecluster2
	./ecluster3
	./ecluster4
	./ecluster5
	
	Eshell V6.4  (abort with ^G)
 	(ecluster4@127.0.0.1)1> nodes().
    ['ecluster1@127.0.0.1','ecluster3@127.0.0.1','ecluster2@127.0.0.1','ecluster5@127.0.0.1']
	
	
	
	
 
	
   
   
   
   
   
 
   
