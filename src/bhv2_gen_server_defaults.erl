%%%========================================================================
%%% File: bhv2_gen_server_defaults.erl
%%%
%%% Default implementations for the callbacks defined by Erlang/OTP's
%%% gen_server behaviour.
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
-module(bhv2_gen_server_defaults).

%% Avoid 'unused function' warnings
-compile([export_all]).


%% ========================================================================
%%  Record definitions
%% ========================================================================

-record(state, {}).

%% ========================================================================
%%  Type definitions
%% ========================================================================

-type state() :: #state{}.


%% ========================================================================
%%  gen_server's callbacks
%% ========================================================================

-spec init(Args :: []) ->
          {'ok', state()} |
          {'ok', state(), timeout()} |
          'ignore' |
          {'stop', Reason :: term()}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: state()) ->
          {'reply', Reply :: term(), state()} |
          {'reply', Reply :: term(), state(), timeout()} |
          {'noreply', state()} |
          {'noreply', state(), timeout()} |
          {'stop', Reason :: term(), Reply :: term(), state()} |
          {'stop', Reason :: term(), state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Msg :: term(),
                  State :: state()) ->
          {'noreply', state()} |
          {'noreply', state(), timeout()} |
          {'stop', Reason :: term(), state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info :: term(),
                  State :: state()) ->
          {'noreply', state()} |
          {'noreply', state(), timeout()} |
          {'stop', Reason :: term(), state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: term(),
                State :: state()) ->
          any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term() | {down, Vsn :: term()},
                  State :: state(),
                  Extra :: term()) ->
          {'ok', NewState :: state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
