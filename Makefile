PROJECT=bhv2

DEPS=meta
dep_meta= git https://github.com/efcasado/meta master

TEST_DIR = tests
TEST_COMPILE_FIRST = dummy_behaviour

include erlang.mk

TEST_ERLC_OPTS := $(TEST_ERLC_OPTS) -pa $(TEST_DIR)
