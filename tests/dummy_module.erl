-module(dummy_module).

-compile([{parse_transform, bhv2_pt}]).

-behaviour(dummy_behaviour).
