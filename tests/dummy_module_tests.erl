%%%========================================================================
%%% File: dummy_module_tests.erl
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
-module(dummy_module_tests).

-include_lib("eunit/include/eunit.hrl").

callbacks_injected_test() ->
    default_f1 = dummy_module:f1(),
    custom_f2 = dummy_module:f2(),
    custom_f2 = dummy_module:f3().

types_injected_test() ->
    %% Inherited attribute
    {T1, _T1Deps} =
        meta:type(t1, 0, dummy_module),
    {attribute, _, type, {t1, {atom, _, default_f1}, _}} = T1,

    %% Overwritten attribute
    {T2, _T2Deps} =
        meta:type(t2, 0, dummy_module),
    {attribute, _, type, {t2, {atom, _, custom_f2}, _}} = T2.
