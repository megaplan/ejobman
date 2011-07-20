EXT_MOD = ../amqp_client
EXT_MOD_INCLUDES = $(EXT_MOD:%=%/include)
INCLUDE_DIR = include
INCLUDE_DIR += $(EXT_MOD_INCLUDES)
INCLUDES = $(INCLUDE_DIR:%=-I%)
SRC_DIR = src
TEST_DIR = test
EBIN_DIR := ebin
ERLC_OPTS = +debug_info -DTEST
ERLC := erlc $(ERLC_OPTS)
VSN=1.0
APP_NAME=ejobman

all: $(EBIN_DIR)
	$(ERLC) -W $(INCLUDES) -o $(EBIN_DIR) $(SRC_DIR)/*.erl
	cp $(SRC_DIR)/ejobman.app.src $(EBIN_DIR)/ejobman_app.app
	
tests: $(EBIN_DIR)
	@$(ERLC) -W $(INCLUDES) -o $(EBIN_DIR) $(TEST_DIR)/*.erl
	
clean:
	@rm -rvf $(EBIN_DIR)/*

ctags:
	cd $(SRC_DIR) ; ctags -R . ../include 

$(EBIN_DIR) :
	( test -d $(EBIN_DIR) || mkdir -p $(EBIN_DIR) )

dia:
	PATH=$(HOME)/util/erlang/dist/r14b3/bin:$(PATH) \
	dialyzer \
		$(INCLUDES) \
		--src \
		-r $(SRC_DIR)

doc:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
		'"."' \
		'[{dir,"doc/html"},{new, true},{hidden, true},{private, true},{def,{vsn,"$(VSN)"}}]'

.PHONY: clean ctags dia doc
