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
    %% Get a list of all the behaviours used by the module being parsed
    Behaviours = meta:behaviours(Forms),

    %% Get a list of all the callbacks that must be implemented by the
    %% module being parsed, according to the list of behaviours used
    %% by the module.
    Callbacks = callbacks(Behaviours),

    %% Automatically export all callbacks
    Forms1 = export_callbacks(Callbacks, Forms),

    %% Get the default implementations of the required callback functions
    Defaults = defaults(Behaviours),

    inject_defaults(Defaults, Forms1).


%% ========================================================================
%%  Local functions
%% ========================================================================

callbacks(Behaviours) ->
    lists:flatmap(fun(B) -> meta:callbacks(B) end, Behaviours).

export_callbacks(Callbacks, Forms) ->
    lists:foldl(fun({F, A}, Forms1) ->
                        %% To avoid 'function X already exported' warnings
                        case meta:is_function_exported(F, A, Forms1) of
                            true ->
                                Forms1;
                            false ->
                                meta:export_function(F, A, Forms1)
                        end
                end,
                Forms,
                Callbacks).

%%-------------------------------------------------------------------------
%% @doc
%% Return the default implementations of all the callbacks defined by the
%% specified behaviours.
%%
%% The returned abstract code is not limited to the callback function only,
%% but it also includes the abstract code of all record, type and function
%% specifications required by the callback function.
%% @end
%%-------------------------------------------------------------------------
defaults(Behaviours) ->
    lists:flatmap(fun(B) ->
                          %% Module where the default implementations are
                          DefaultsModule = defaults_module(B),
                          %% All callbacks required by the behaviour
                          Callbacks = meta:callbacks(B),
                          %% Return only those callbacks there is a default
                          %% implementation for
                          defaults(Callbacks, DefaultsModule)
                  end,
                  Behaviours).

defaults(Callbacks, DefaultsModule) ->
    lists:zf(
      fun({F, A}) ->
              try
                  Forms = meta:function(F, A, DefaultsModule, [all, abstract]),
                  {true, Forms}
              catch
                  _:_ -> false
              end
      end,
      Callbacks).

inject_defaults([], ModuleForms) ->
    ModuleForms;
inject_defaults([CallbackDefaults| Callbacks], ModuleForms) ->
    Forms = prepare_callback_defaults(CallbackDefaults, ModuleForms),
    ModuleForms1 = meta:add_forms(Forms, ModuleForms),
    inject_defaults(Callbacks, ModuleForms1).

prepare_callback_defaults({Function, _Spec = undefined, Deps}, ModuleForms) ->
    filter_out_duplicates([Function| Deps], ModuleForms);
prepare_callback_defaults({Function, Spec, Deps}, ModuleForms) ->
    filter_out_duplicates([Function, Spec| Deps], ModuleForms).

%% Filter out those forms that have already been provided by the user in
%% the module being parsed
filter_out_duplicates(Forms, ModuleForms) ->
    '_filter_out_duplicates'(Forms, [], ModuleForms).

'_filter_out_duplicates'([], Acc, _Forms) ->
    lists:reverse(Acc);
'_filter_out_duplicates'([D = {attribute, _, record, {Record, _}}| Ds],
                         Acc, Forms) ->
    Acc1 = case meta:has_record(Record, Forms) of
               true ->
                   Acc;
               false ->
                   [D| Acc]
           end,
    '_filter_out_duplicates'(Ds, Acc1, Forms);
'_filter_out_duplicates'([D = {attribute, _, type, {Name, _, Args}}| Ds],
                         Acc, Forms) ->
    Acc1 = case meta:has_type(Name, length(Args), Forms) of
               true ->
                   Acc;
               false ->
                   [D| Acc]
           end,
    '_filter_out_duplicates'(Ds, Acc1, Forms);
'_filter_out_duplicates'([D = {attribute, _, spec, {{Name, Arity}, _}}| Ds],
                         Acc, Forms) ->
    Acc1 = case meta:has_type(Name, Arity, Forms) of
               true ->
                   Acc;
               false ->
                   [D| Acc]
           end,
    '_filter_out_duplicates'(Ds, Acc1, Forms);
'_filter_out_duplicates'([D = {function, _, Name, Arity, _}| Ds],
                         Acc, Forms) ->
    Acc1 = case meta:has_function(Name, Arity, Forms) of
               true ->
                   Acc;
               false ->
                   [D| Acc]
           end,
    '_filter_out_duplicates'(Ds, Acc1, Forms).



defaults_module(gen_server) ->
    bhv2_gen_server_defaults;
defaults_module(Module) ->
    Module.
