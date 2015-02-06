-module(dummy_gen_server).

-compile({parse_transform, bhv2_pt}).

-behaviour(gen_server).

-export([init/1, handle_info/2]).


%% Only init/1 and handle_info/2 are provided by the user.

init([Owner]) ->
    {ok, Owner}.

handle_info(Info, Owner) ->
    Owner ! lists:reverse(Info),
    {noreply, Owner}.
