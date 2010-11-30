BIN=ebin

ERL_DIR=$(shell erl -noshell -eval 'io:format("~s~n", [code:root_dir()])' -s erlang halt)
EI_DIR=$(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir("erl_interface")])' -s erlang halt)
EI_INC_DIR= -I $(ERL_DIR)/usr/include -I $(EI_DIR)/include
LDFLAGS=-fpic -shared -L $(EI_DIR)/lib -L $(ERL_DIR)/usr/lib -lei -lerl_interface 
LAN_INTF=eth0
IP_ADDR=$(shell /sbin/ifconfig $(LAN_INTF) | grep "inet addr" | tr : \ | awk '{print $3}')

all: compile

compile: 
	@mkdir -p ebin
	@erl -make
	@find . -wholename "*/src/*.app" -exec cp -f {} ebin/ \;

clean: 
	@rm -rf ebin/*.beam 
	@rm -rf ebin/*.app 

shell:
	@erl $(BIN)

create-schema:
	scripts/run.sh cacherl -noshell -s init_schema -s erlang halt
