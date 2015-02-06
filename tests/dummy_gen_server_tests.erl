-module(dummy_gen_server_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, dummy).


simple_test() ->
    gen_server:start({local, ?SERVER}, dummy_gen_server, [self()], []),

    ok = gen_server:call(?SERVER, oops),

    ?SERVER ! "foo",
    "oof" = receive Msg -> Msg end.
