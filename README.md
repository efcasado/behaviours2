behaviours2
===========

Erlang behaviours on steroids.


`behaviours2` allows developers to provide sound defaults for a behaviour's
callbacks. `behaviours2`'s parse transform will automatically inject a
callback's default implementation unless the user overwrites it by providing
a custom implementation.

### Examples

##### Simple behaviour

```erlang
-module(my_awesome_behaviour).

-export([export_all]).

-type t1() :: any().
-type t2() :: any().

-callback f1() -> t1().
-callback f2() -> t2().
-callback f3() -> t2().

f1() ->
    'default_f1'.

f2() ->
    'default_f2'.
    
f3() ->
    f2().
```

```erlang
-module(my_awesome_module).

-compile({parse_transform, bhvs2_pt}).

-behaviour(my_awesome_behaviour).

f2() ->
  'custom_f2'.

```

```erlang
my_awesome_module:f1().
% => default_f1

my_awesome_module:f2().
% => custom_f2

my_awesome_module:f3().
% => custom_f2.
```

Note that `f1` and `f3` were automatically injected into `my_awesome_module`. Note as well that no
explicit exports for the injected callback functions nor the provided one were required.

##### gen_server (echo server)

The code snippet below illustrates how much effort it would take to
write an echo server.

```erlang
-module(echo_server).

-compile({parse_transform, bhv2_pt}).

-behaviour(gen_server).

-export([handle_call/3]).

handle_call(Msg, From, State) ->
  Reply = Msg,
  {reply, Msg, State}.

```

Below is its plain Erlang/OTP counterpart.

```erlang
-module(echo_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {}).

-type state() :: #state{}.

-spec init(Args :: []) ->
          {ok, state()} |
          {ok, state(), timeout()} |
          ignore |
          {stop, Reason :: term()}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: state()) ->
          {reply, Reply :: term(), state()} |
          {reply, Reply :: term(), state(), timeout()} |
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, Reason :: term(), Reply :: term(), state()} |
          {stop, Reason :: term(), state()}.
handle_call(Msg, _From, State) ->
    Reply = Msg,
    {reply, Reply, State}.

-spec handle_cast(Msg :: term(),
                  State :: state()) ->
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, Reason :: term(), state()}.
handle_cast(Msg, State) ->
        {noreply, State}.

-spec handle_info(Info :: term(),
                  State :: state()) ->
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, Reason :: term(), state()}.
handle_info(Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: term(),
                State :: state()) -> any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term() | {down, Vsn :: term()},
                  State :: state(),
                  Extra :: term()) ->
          {ok, NewState :: state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

Quite a significant difference. Don't you think?
