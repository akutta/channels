-module(channels_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("channels/include/channels.hrl").

-compile(export_all).

%% Tests
init_test() ->
  given_app_not_started(),
  when_app_started(),
  then_table_created(),
  teardown().

add_test() ->
  given_app_started(),
  when_channels_added([{?MODULE, func1, 10},
    {?MODULE, func2, 20},
    {?MODULE, func3, 5}]),
  then_channels_retrieved([{?MODULE, func3, [], 5},
    {?MODULE, func1, [], 10},
    {?MODULE, func2, [], 20}]),
  teardown().

add_duplicate_test() ->
  given_app_started(),
  when_channels_added([{?MODULE, func1, 10},
    {?MODULE, func2, 5},
    {?MODULE, func1, 1}]),
  then_channels_retrieved([{?MODULE, func1, [], 1},
    {?MODULE, func2, [], 5}]),
  teardown().

delete_test() ->
  given_app_started(),
  when_channels_added([{?MODULE, func1, 10},
    {?MODULE, func2, 20},
    {?MODULE, func3, 5},
    {?MODULE, func4, 30}]),
  when_channels_deleted([{?MODULE, func1},
    {?MODULE, func4}]),
  then_channels_retrieved([{?MODULE, func3, [], 5},
    {?MODULE, func2, [], 20}]),
  teardown().

run_test() ->
  given_app_started(),
  given_no_run_results(),
  when_channels_added([{?MODULE, func1, 2},
    {?MODULE, func2, 3},
    {?MODULE, func3, 1}]),
  then_run_result([3, 1, 2]),
  teardown().

run_arg_test() ->
  given_app_started(),
  given_no_run_results(),
  when_channels_arg_added([{?MODULE, func_arg, [a], 3},
    {?MODULE, func_arg, [b], 1},
    {?MODULE, func_arg, [c], 2}]),
  then_run_result([{b}, {c}, {a}]),
  teardown().

run_undefined_test() ->
  given_app_started(),
  given_no_run_results(),
  when_channels_added([]),
  then_run_result(undefined),
  teardown().

run_default_test() ->
  given_app_started(),
  given_no_run_results(),
  when_channels_added([]),
  Default = {this, is, a, default, value},
  then_default_value_run_result(Default, Default),
  teardown().

run_stop_test() ->
  given_app_started(),
  given_no_run_results(),
  when_channels_added([{?MODULE, func_stop, 1}, {?MODULE, func1, 2}]),
  then_run_result("this is a stop"),
  teardown().

%% Callbacks
func1() -> func(1).
func2() -> func(2).
func3() -> func(3).

func_arg(Arg) ->
  R = case erlang:get(run_results) of
        undefined -> [{Arg}];
        Results -> Results ++ [{Arg}]
      end,
  erlang:put(run_results, R),
  R.

func_stop() -> {stop, "this is a stop"}.

func(N) ->
  R = case erlang:get(run_results) of
        undefined -> [N];
        Results -> Results ++ [N]
      end,
  erlang:put(run_results, R),
  R.

%% Helpers
teardown() ->
  application:stop(channels).

given_app_not_started() ->
  application:stop(channels).

given_app_started() ->
  application:ensure_all_started(channels),
  ets:delete_all_objects(channels_table).

given_no_run_results() ->
  erlang:erase(run_results).

when_channels_arg_added(Channels) ->
  [channels:add(test_channel, M, F, A, P) ||
    {M, F, A, P} <- Channels].

when_channels_added(Channels) ->
  [channels:add(test_channel, M, F, P) || {M, F, P} <- Channels].

when_channels_deleted(Channels) ->
  [channels:delete(test_channel, M, F) || {M, F} <- Channels].

then_run_result(Result) ->
  ?assertEqual(Result, channels:run(test_channel, [])).

then_default_value_run_result(Default, Result) ->
  ?assertEqual(Result, channels:run(test_channel, [], Default)).

then_channels_retrieved(Channels) ->
  [{test_channel, ChannelsRecords}] = ets:lookup(channels_table, test_channel),
  ?assertEqual(Channels, channels_to_tuples(ChannelsRecords)).

when_app_started() ->
  application:ensure_all_started(channels).

then_table_created() ->
  ?assertNotEqual(undefined, ets:info(channels_table)).

channels_to_tuples(Records) ->
  [{M, F, A, Prio} || #channel{mfa = {M, F, A}, priority = Prio} <- Records].
