-module(erlzord_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.


-spec '10d_from_minus123456789_to_54321_test_'() -> fun(() -> ok).
'10d_from_minus123456789_to_54321_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 10,
                min_component => -123456789,
                max_component => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(10, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/10dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 10, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 10, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 10, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 10, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 10, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '10d_from_0_to_100_test_'() -> fun(() -> ok).
'10d_from_0_to_100_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 10,
                min_component => 0,
                max_component => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(10, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/10dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 10, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 10, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 10, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 10, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 10, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '10d_from_0_to_123456789_test_'() -> fun(() -> ok).
'10d_from_0_to_123456789_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 10,
                min_component => 0,
                max_component => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(10, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/10dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 10, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 10, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 10, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 10, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 10, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '10d_from_0_to_7_test_'() -> fun(() -> ok).
'10d_from_0_to_7_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 10,
                min_component => 0,
                max_component => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(10, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/10dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 10, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 10, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 10, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 10, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 10, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '1d_from_minus123456789_to_54321_test_'() -> fun(() -> ok).
'1d_from_minus123456789_to_54321_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 1,
                min_component => -123456789,
                max_component => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(1, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/1dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 1, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 1, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 1, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 1, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 1, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '1d_from_0_to_100_test_'() -> fun(() -> ok).
'1d_from_0_to_100_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 1,
                min_component => 0,
                max_component => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(1, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/1dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 1, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 1, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 1, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 1, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 1, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '1d_from_0_to_123456789_test_'() -> fun(() -> ok).
'1d_from_0_to_123456789_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 1,
                min_component => 0,
                max_component => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(1, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/1dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 1, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 1, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 1, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 1, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 1, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '1d_from_0_to_7_test_'() -> fun(() -> ok).
'1d_from_0_to_7_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 1,
                min_component => 0,
                max_component => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(1, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/1dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 1, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 1, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 1, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 1, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 1, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '2d_from_minus123456789_to_54321_test_'() -> fun(() -> ok).
'2d_from_minus123456789_to_54321_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 2,
                min_component => -123456789,
                max_component => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(2, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/2dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 2, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 2, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 2, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 2, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 2, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '2d_from_0_to_100_test_'() -> fun(() -> ok).
'2d_from_0_to_100_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 2,
                min_component => 0,
                max_component => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(2, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/2dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 2, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 2, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 2, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 2, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 2, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '2d_from_0_to_123456789_test_'() -> fun(() -> ok).
'2d_from_0_to_123456789_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 2,
                min_component => 0,
                max_component => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(2, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/2dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 2, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 2, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 2, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 2, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 2, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '2d_from_0_to_7_test_'() -> fun(() -> ok).
'2d_from_0_to_7_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 2,
                min_component => 0,
                max_component => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(2, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/2dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 2, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 2, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 2, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 2, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 2, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '30d_from_minus123456789_to_54321_test_'() -> fun(() -> ok).
'30d_from_minus123456789_to_54321_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 30,
                min_component => -123456789,
                max_component => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(30, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/30dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 30, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 30, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 30, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 30, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 30, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '30d_from_0_to_100_test_'() -> fun(() -> ok).
'30d_from_0_to_100_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 30,
                min_component => 0,
                max_component => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(30, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/30dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 30, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 30, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 30, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 30, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 30, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '30d_from_0_to_123456789_test_'() -> fun(() -> ok).
'30d_from_0_to_123456789_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 30,
                min_component => 0,
                max_component => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(30, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/30dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 30, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 30, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 30, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 30, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 30, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '30d_from_0_to_7_test_'() -> fun(() -> ok).
'30d_from_0_to_7_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 30,
                min_component => 0,
                max_component => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(30, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/30dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 30, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 30, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 30, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 30, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 30, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '3d_from_minus123456789_to_54321_test_'() -> fun(() -> ok).
'3d_from_minus123456789_to_54321_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 3,
                min_component => -123456789,
                max_component => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(3, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/3dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 3, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 3, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 3, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 3, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 3, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '3d_from_0_to_100_test_'() -> fun(() -> ok).
'3d_from_0_to_100_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 3,
                min_component => 0,
                max_component => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(3, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/3dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 3, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 3, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 3, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 3, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 3, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '3d_from_0_to_123456789_test_'() -> fun(() -> ok).
'3d_from_0_to_123456789_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 3,
                min_component => 0,
                max_component => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(3, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/3dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 3, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 3, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 3, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 3, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 3, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '3d_from_0_to_7_test_'() -> fun(() -> ok).
'3d_from_0_to_7_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 3,
                min_component => 0,
                max_component => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(3, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/3dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 3, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 3, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 3, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 3, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 3, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '4d_from_minus123456789_to_54321_test_'() -> fun(() -> ok).
'4d_from_minus123456789_to_54321_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 4,
                min_component => -123456789,
                max_component => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(4, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/4dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 4, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 4, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 4, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 4, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 4, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '4d_from_0_to_100_test_'() -> fun(() -> ok).
'4d_from_0_to_100_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 4,
                min_component => 0,
                max_component => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(4, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/4dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 4, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 4, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 4, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 4, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 4, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '4d_from_0_to_123456789_test_'() -> fun(() -> ok).
'4d_from_0_to_123456789_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 4,
                min_component => 0,
                max_component => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(4, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/4dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 4, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 4, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 4, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 4, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 4, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '4d_from_0_to_7_test_'() -> fun(() -> ok).
'4d_from_0_to_7_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 4,
                min_component => 0,
                max_component => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(4, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/4dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 4, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 4, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 4, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 4, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 4, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '5d_from_minus123456789_to_54321_test_'() -> fun(() -> ok).
'5d_from_minus123456789_to_54321_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 5,
                min_component => -123456789,
                max_component => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(5, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/5dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 5, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 5, -123456789, 54321),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 5, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 5, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 5, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '5d_from_0_to_100_test_'() -> fun(() -> ok).
'5d_from_0_to_100_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 5,
                min_component => 0,
                max_component => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(5, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/5dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 5, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 5, 0, 100),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 5, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 5, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 5, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '5d_from_0_to_123456789_test_'() -> fun(() -> ok).
'5d_from_0_to_123456789_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 5,
                min_component => 0,
                max_component => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(5, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/5dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 5, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 5, 0, 123456789),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 5, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 5, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 5, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-spec '5d_from_0_to_7_test_'() -> fun(() -> ok).
'5d_from_0_to_7_test_'() ->
    {
        timeout,
        10,
        fun () ->
            Config = erlzord:config(#{
                dimensions => 5,
                min_component => 0,
                max_component => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(5, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/5dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Vector, ExpectedValue}) ->
                    Value = erlzord:encode(Vector, Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Vector), Config),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, ListValue}),


                    DirectValue = erlzord:encode(Vector, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Vector), 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Vector, 5, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Vector), 5, 0, 7),
                    ?assertEqual({Vector, ExpectedValue}, {Vector, LegacyDirectListValue}),


                    DecodedTupleVector = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleVector}, {Value, Vector}),

                    DecodedListVector = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListVector)}, {Value, Vector}),

                    DecodedLegacyTupleVector = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleVector}, {Value, Vector}),


                    DirectDecodedTupleVector = erlzord:decode_tuple(Value, 5, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleVector}, {Value, Vector}),

                    DirectDecodedListVector = erlzord:decode_list(Value, 5, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListVector)}, {Value, Vector}),

                    DirectDecodedLegacyTupleVector = erlzord:decode(Value, 5, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleVector}, {Value, Vector})
                end,
                Terms)
        end
    }.

-endif.