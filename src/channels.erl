-module(channels).

%% API
-export([init_table/0,
  add/4,
  add/5,
  delete/3,
  delete/4,
  run/2,
  run/3]).

-define(TABLE, channels_table).

-include("channels.hrl").

-type channel_name() :: atom().
-type args() :: [any()].

%% ===================================================================
%% API functions
%% ===================================================================
-spec init_table() -> ok.
init_table() ->
  ?TABLE = ets:new(?TABLE, [named_table, public, {read_concurrency, true}]),
  ok.

-spec add(channel_name(), module(), atom(), non_neg_integer()) -> ok.
add(Name, Module, Function, Priority) ->
  add(Name, Module, Function, [], Priority).

-spec add(channel_name(), module(), atom(), args(), non_neg_integer()) -> ok.
add(Name, Module, Function, Args, Priority) ->
  Channels1 = channels(Name),
  Channels2 = do_add(Channels1, Module, Function, Args, Priority),
  store(Name, Channels2).

-spec delete(channel_name(), module(), atom()) -> ok.
delete(Name, Module, Function) ->
  delete(Name, Module, Function, []).

-spec delete(channel_name(), module(), atom(), args()) -> ok.
delete(Name, Module, Function, Args) ->
  Channels1 = channels(Name),
  Channels2 = do_delete(Channels1, Module, Function, Args),
  store(Name, Channels2).

-spec run(channel_name(), args()) -> any().
run(Name, Args) ->
  run(Name, Args, undefined).

-spec run(channel_name(), args(), any()) -> any().
run(Name, Args, Default) ->
  Channels = channels(Name),
  do_run(Channels, Args, Default).

%% ===================================================================
%% Internal functions
%% ===================================================================
channels(Name) ->
  case ets:lookup(?TABLE, Name) of
    [{Name, Channels}] -> Channels;
    [] -> []
  end.

do_run(Channels, Args, Default) ->
  F = fun(#channel{mfa = {M, F, A}}, _) ->
    case apply(M, F, A ++ Args) of
      {stop, Value} ->
        erlang:throw({stop, Value});
      Value ->
        Value
    end
      end,
  try
    lists:foldl(F, Default, Channels)
  catch throw:{stop, Value} ->
    Value
  end.

store(Name, Channels) ->
  true = ets:insert(?TABLE, {Name, Channels}),
  ok.

do_add(Channels0, Module, Function, Args, Priority) ->
  MFA = {Module, Function, Args},
  Channel = #channel{mfa = MFA, priority = Priority},
  Channels1 = lists:keystore(MFA, #channel.mfa, Channels0, Channel),
  sort(Channels1).

do_delete(Channels0, Module, Function, Args) ->
  lists:keydelete({Module, Function, Args}, #channel.mfa, Channels0).

sort(Channels) ->
  lists:keysort(#channel.priority, Channels).
