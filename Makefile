PROJECT = term_validator
PROJECT_DESCRIPTION = A quick and simple Erlang term validator.
PROJECT_VERSION = 0.1.0

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck 0.9.2

include erlang.mk
