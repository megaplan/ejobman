EXT_MOD = ../amqp_client
EXT_MOD_INCLUDES = $(EXT_MOD:%=%/include)
INCLUDE_DIR = include
INCLUDE_DIR += $(EXT_MOD_INCLUDES)
#INCLUDE_DIR += ../proper/include ..
INCLUDES = $(INCLUDE_DIR:%=-I%)
SRC_DIR = src
TEST_DIR = test
EBIN_DIR := ebin
HTML_DOC_DIR = doc/html
ERLC_OPTS = +debug_info -DTEST
#ERLC_OPTS = +debug_info -DTEST -DPROPER -pa ../proper/ebin
ERLC := erlc $(ERLC_OPTS)
VSN=2.1
APP_NAME=ejobman
LICENSE=MIT

all: $(EBIN_DIR)
	$(ERLC) -W $(INCLUDES) -o $(EBIN_DIR) $(SRC_DIR)/*.erl
	cp $(SRC_DIR)/ejobman.app.src $(EBIN_DIR)/ejobman.app

tests: $(EBIN_DIR)
	@$(ERLC) -W $(INCLUDES) -o $(EBIN_DIR) $(TEST_DIR)/*.erl

clean:
	@rm -rvf $(EBIN_DIR)/* $(HTML_DOC_DIR)

tags: ctags etags

ctags:
	cd $(SRC_DIR) ; ctags -R . ../include 

etags:
	cd $(SRC_DIR) ; etags -R . ../include 

$(EBIN_DIR) :
	( test -d $(EBIN_DIR) || mkdir -p $(EBIN_DIR) )

dia:
	dialyzer \
		-Wrace_conditions \
		-Werror_handling \
		$(INCLUDES) \
		--src \
		-r $(SRC_DIR)

dia2:
	dialyzer \
		-Wunmatched_returns \
		-Wrace_conditions \
		-Werror_handling \
		$(INCLUDES) \
		--src \
		-r $(SRC_DIR)

doc:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
		'"."' \
		'[{dir,"$(HTML_DOC_DIR)"},{new, true},{hidden, true},{private, true},{def,[{vsn,"$(VSN)"}, {license, "(License: $(LICENSE))"}]}]'

.PHONY: clean ctags dia doc
