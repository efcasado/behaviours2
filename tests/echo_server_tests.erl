%%%========================================================================
%%% File: echo_server_tests.erl
%%%
%%% Unit tests for the 'echo_server' module.
%%%
%%%
%%% Author: Enrique Fernandez <enrique.fernandez@gmail.com>
%%% Date:   February, 2015
%%%
%%%-- LICENSE -------------------------------------------------------------
%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2015 Enrique Fernandez
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do so,
%%% subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%========================================================================
-module(echo_server_tests).

-include_lib("eunit/include/eunit.hrl").

%% ========================================================================
%%  Macro definitions
%% ========================================================================

-define(GEN_SERVER_CALLBACKS,
        %% All callbacks defined in the gen_server module
        [
         {init,1},
         {handle_call,3},
         {handle_cast,2},
         {handle_info,2},
         {terminate,2},
         {code_change,3}
        ]).

-define(GEN_SERVER_RECORDS,
        %% All callbacks defined in the gen_server module
        [
         state
        ]).


%% ========================================================================
%%  Unit tests
%% ========================================================================

call_test() ->
    Pid = start(),
    <<"Hello, world!">> = call(<<"Hello, world!">>, Pid),
    ok = stop(Pid).

callbacks_injected_test() ->
    EchoServerFunctions = meta:functions(echo_server),
    true = lists:all(fun(F) ->
                             lists:member(F, EchoServerFunctions)
                     end,
                     ?GEN_SERVER_CALLBACKS).

callbacks_exported_test() ->
    true = lists:all(fun({F,A}) ->
                             meta:is_function_exported(F, A, echo_server)
                     end,
                     ?GEN_SERVER_CALLBACKS).

specs_injected_test() ->
    EchoServerSpecs = meta:specs(echo_server),
    true = lists:all(fun(F) ->
                            lists:member(F, EchoServerSpecs)
                    end,
                    ?GEN_SERVER_CALLBACKS).

records_injected_test() ->
    EchoServerRecords = meta:records(echo_server),
    true = lists:all(fun(R) ->
                            lists:member(R, EchoServerRecords)
                    end,
                    ?GEN_SERVER_RECORDS).


%% ========================================================================
%%  Local functions
%% ========================================================================

start() ->
    {ok, Pid} = gen_server:start(echo_server, [], []),
    Pid.

call(Message, Pid) ->
    gen_server:call(Pid, Message).

stop(Pid) ->
    true = exit(Pid, kill),
    ok.
