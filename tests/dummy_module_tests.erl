-module(dummy_module_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    foo = dummy_module:foo().
