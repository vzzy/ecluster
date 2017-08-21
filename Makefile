REBAR = ./rebar
COOKIE = ecluster

all:deps clean compile rund
 
compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps
   
clean:
	rm -rf ebin/*
	$(REBAR) clean
	
start:
	erl -pa ebin -name 'ecluster@127.0.0.1' -setcookie $(COOKIE) -config etc/ecluster.conf -s $(COOKIE) -detached
   
run:
	erl -pa ebin -name 'ecluster@127.0.0.1' -setcookie $(COOKIE) -config etc/ecluster.conf -s $(COOKIE)
	
rund:
	erl -pa ebin -name 'ecluster@127.0.0.1' -setcookie $(COOKIE) -config etc/ecluster.config -s $(COOKIE)
	
issue:deps clean compile
	rm -rf release
	rm -rf ecluster.tar.gz
	mkdir -p release/ebin
	mkdir -p release/etc
	cp -f -R ebin release
	cp -f -R etc release
	cp -f README.md release/
	echo "erl -pa ebin -name 'ecluster1@127.0.0.1' -setcookie $(COOKIE) -config etc/ecluster.config -s $(COOKIE)" >> release/ecluster1
	echo "erl -pa ebin -name 'ecluster2@127.0.0.1' -setcookie $(COOKIE) -config etc/ecluster.config -s $(COOKIE)" >> release/ecluster2
	echo "erl -pa ebin -name 'ecluster3@127.0.0.1' -setcookie $(COOKIE) -config etc/ecluster.config -s $(COOKIE)" >> release/ecluster3
	echo "erl -pa ebin -name 'ecluster4@127.0.0.1' -setcookie $(COOKIE) -config etc/ecluster.config -s $(COOKIE)" >> release/ecluster4
	echo "erl -pa ebin -name 'ecluster5@127.0.0.1' -setcookie $(COOKIE) -config etc/ecluster.config -s $(COOKIE)" >> release/ecluster5
	chmod +x release/ecluster*
	tar cvf ecluster.tar.gz release
	
.PHONY:all deps compile clean start run rund cluster issue


