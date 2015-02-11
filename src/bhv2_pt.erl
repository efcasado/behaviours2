%%%========================================================================
%%% File: bhv2_pt.erl
%%%
%%% Parse transform that automatically injects sound callback defaults
%%% when no user-provided implementations are found in the module
%%% implementing the behaviour.
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
-module(bhv2_pt).

-export([parse_transform/2]).


%% ========================================================================
%%  Parse transform
%% ========================================================================

parse_transform(Forms, _Opts) ->
    Callbacks = all_default_callbacks(Forms),
    inject_callbacks(Callbacks, Forms).


%% ========================================================================
%%  Local functions
%% ========================================================================

all_default_callbacks(Forms) ->
    lists:flatmap(fun({attribute, _, behaviour, Behaviour})
                        when is_atom(Behaviour) ->
                          Callbacks = callbacks(Behaviour),
                          default_callbacks(Callbacks, Behaviour);
                     (_) ->
                          []
                  end,
                  Forms).

callbacks(Behaviour)
  when is_atom(Behaviour) ->
    BehaviourForms = forms:read(Behaviour),
    lists:zf(fun({attribute, _, callback, {Callback, _}}) -> {true, Callback};
                (_) -> false
             end,
             BehaviourForms).

default_callbacks(Callbacks, Behaviour) ->
    DefaultsModule = defaults_module(Behaviour),
    lists:zf(
      fun({F, A} = Callback) ->
              try
                  Function =
                      meta:function(F, A, DefaultsModule, [all, abstract]),
                  {true, {Callback, Function}}
              catch
                  _:_ -> false
              end
      end,
      Callbacks).

inject_callbacks([], Forms) ->
    Forms;
inject_callbacks([{{F, A}, Callback}| Callbacks], Forms) ->
    case meta:has_function(F, A, Forms) of
        true ->
            inject_callbacks(Callbacks, Forms);
        false ->
            {{function, _, Name, Arity, _}, _, _} = Callback,
            Forms1 = meta:add_forms(prepare_callback(Callback), Forms),
            Forms2 = meta:export_function(Name, Arity, Forms1),
            inject_callbacks(Callbacks, Forms2)
    end.

prepare_callback({Function, Spec = undefined, Deps}) ->
    [Function| Deps];
prepare_callback({Function, Spec, Deps}) ->
    [Function, Spec| Deps].

defaults_module(gen_server) ->
    bhv2_gen_server_defaults;
defaults_module(Module) ->
    Module.
