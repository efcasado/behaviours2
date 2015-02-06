-module(dummy_behaviour).

-compile([export_all]).

-callback foo() -> 'foo'.

foo() -> foo.
