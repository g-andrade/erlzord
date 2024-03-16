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
                dimension => 10,
                min_coordinate_value => -123456789,
                max_coordinate_value => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 10,
                    min_coordinate_value => -123456789,
                    max_coordinate_value => 54321
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(10, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/10dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 10, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 10, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 10, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 10, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 10, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 10,
                min_coordinate_value => 0,
                max_coordinate_value => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 10,
                    min_coordinate_value => 0,
                    max_coordinate_value => 100
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(10, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/10dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 10, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 10, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 10, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 10, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 10, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 10,
                min_coordinate_value => 0,
                max_coordinate_value => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 10,
                    min_coordinate_value => 0,
                    max_coordinate_value => 123456789
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(10, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/10dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 10, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 10, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 10, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 10, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 10, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 10,
                min_coordinate_value => 0,
                max_coordinate_value => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 10,
                    min_coordinate_value => 0,
                    max_coordinate_value => 7
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(10, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/10dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 10, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 10, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 10, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 10, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 10, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 1,
                min_coordinate_value => -123456789,
                max_coordinate_value => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 1,
                    min_coordinate_value => -123456789,
                    max_coordinate_value => 54321
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(1, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/1dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 1, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 1, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 1, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 1, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 1, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 1,
                min_coordinate_value => 0,
                max_coordinate_value => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 1,
                    min_coordinate_value => 0,
                    max_coordinate_value => 100
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(1, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/1dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 1, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 1, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 1, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 1, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 1, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 1,
                min_coordinate_value => 0,
                max_coordinate_value => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 1,
                    min_coordinate_value => 0,
                    max_coordinate_value => 123456789
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(1, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/1dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 1, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 1, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 1, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 1, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 1, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 1,
                min_coordinate_value => 0,
                max_coordinate_value => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 1,
                    min_coordinate_value => 0,
                    max_coordinate_value => 7
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(1, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/1dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 1, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 1, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 1, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 1, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 1, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 2,
                min_coordinate_value => -123456789,
                max_coordinate_value => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 2,
                    min_coordinate_value => -123456789,
                    max_coordinate_value => 54321
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(2, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/2dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 2, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 2, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 2, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 2, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 2, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 2,
                min_coordinate_value => 0,
                max_coordinate_value => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 2,
                    min_coordinate_value => 0,
                    max_coordinate_value => 100
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(2, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/2dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 2, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 2, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 2, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 2, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 2, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 2,
                min_coordinate_value => 0,
                max_coordinate_value => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 2,
                    min_coordinate_value => 0,
                    max_coordinate_value => 123456789
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(2, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/2dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 2, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 2, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 2, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 2, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 2, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 2,
                min_coordinate_value => 0,
                max_coordinate_value => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 2,
                    min_coordinate_value => 0,
                    max_coordinate_value => 7
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(2, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/2dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 2, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 2, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 2, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 2, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 2, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 30,
                min_coordinate_value => -123456789,
                max_coordinate_value => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 30,
                    min_coordinate_value => -123456789,
                    max_coordinate_value => 54321
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(30, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/30dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 30, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 30, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 30, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 30, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 30, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 30,
                min_coordinate_value => 0,
                max_coordinate_value => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 30,
                    min_coordinate_value => 0,
                    max_coordinate_value => 100
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(30, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/30dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 30, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 30, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 30, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 30, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 30, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 30,
                min_coordinate_value => 0,
                max_coordinate_value => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 30,
                    min_coordinate_value => 0,
                    max_coordinate_value => 123456789
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(30, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/30dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 30, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 30, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 30, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 30, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 30, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 30,
                min_coordinate_value => 0,
                max_coordinate_value => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 30,
                    min_coordinate_value => 0,
                    max_coordinate_value => 7
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(30, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/30dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 30, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 30, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 30, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 30, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 30, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 3,
                min_coordinate_value => -123456789,
                max_coordinate_value => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 3,
                    min_coordinate_value => -123456789,
                    max_coordinate_value => 54321
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(3, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/3dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 3, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 3, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 3, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 3, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 3, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 3,
                min_coordinate_value => 0,
                max_coordinate_value => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 3,
                    min_coordinate_value => 0,
                    max_coordinate_value => 100
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(3, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/3dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 3, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 3, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 3, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 3, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 3, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 3,
                min_coordinate_value => 0,
                max_coordinate_value => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 3,
                    min_coordinate_value => 0,
                    max_coordinate_value => 123456789
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(3, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/3dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 3, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 3, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 3, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 3, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 3, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 3,
                min_coordinate_value => 0,
                max_coordinate_value => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 3,
                    min_coordinate_value => 0,
                    max_coordinate_value => 7
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(3, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/3dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 3, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 3, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 3, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 3, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 3, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 4,
                min_coordinate_value => -123456789,
                max_coordinate_value => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 4,
                    min_coordinate_value => -123456789,
                    max_coordinate_value => 54321
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(4, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/4dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 4, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 4, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 4, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 4, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 4, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 4,
                min_coordinate_value => 0,
                max_coordinate_value => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 4,
                    min_coordinate_value => 0,
                    max_coordinate_value => 100
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(4, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/4dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 4, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 4, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 4, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 4, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 4, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 4,
                min_coordinate_value => 0,
                max_coordinate_value => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 4,
                    min_coordinate_value => 0,
                    max_coordinate_value => 123456789
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(4, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/4dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 4, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 4, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 4, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 4, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 4, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 4,
                min_coordinate_value => 0,
                max_coordinate_value => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 4,
                    min_coordinate_value => 0,
                    max_coordinate_value => 7
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(4, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/4dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 4, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 4, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 4, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 4, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 4, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 5,
                min_coordinate_value => -123456789,
                max_coordinate_value => 54321
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 5,
                    min_coordinate_value => -123456789,
                    max_coordinate_value => 54321
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(5, -123456789, 54321)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/5dim_from_-123456789_to_54321.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 5, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 5, -123456789, 54321),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 5, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 5, -123456789, 54321),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 5, -123456789, 54321),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 5,
                min_coordinate_value => 0,
                max_coordinate_value => 100
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 5,
                    min_coordinate_value => 0,
                    max_coordinate_value => 100
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(5, 0, 100)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/5dim_from_0_to_100.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 5, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 5, 0, 100),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 5, 0, 100),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 5, 0, 100),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 5, 0, 100),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 5,
                min_coordinate_value => 0,
                max_coordinate_value => 123456789
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 5,
                    min_coordinate_value => 0,
                    max_coordinate_value => 123456789
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(5, 0, 123456789)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/5dim_from_0_to_123456789.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 5, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 5, 0, 123456789),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 5, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 5, 0, 123456789),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 5, 0, 123456789),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
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
                dimension => 5,
                min_coordinate_value => 0,
                max_coordinate_value => 7
            }),

            ?assertEqual(
                Config,
                erlzord:config(#{
                    dimensions => 5,
                    min_coordinate_value => 0,
                    max_coordinate_value => 7
                })
            ),

            ?assertEqual(
                Config,
                erlzord:config(5, 0, 7)
            ),

            {ok, Terms} = file:consult("support/test_cases/test_data/5dim_from_0_to_7.data"),
            lists:foreach(
                fun ({Coordinates, ExpectedValue}) ->
                    Value = erlzord:encode(Coordinates, Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                    ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, ListValue}),


                    DirectValue = erlzord:encode(Coordinates, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                    DirectValueList = erlzord:encode(tuple_to_list(Coordinates), 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValueList}),


                    LegacyDirectValue = erlzord:encode(Coordinates, 5, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectValue}),

                    LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), 5, 0, 7),
                    ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, LegacyDirectListValue}),


                    DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                    ?assertEqual({Value, DecodedTupleCoordinates}, {Value, Coordinates}),

                    DecodedListCoordinates = erlzord:decode_list(Value, Config),
                    ?assertEqual({Value, list_to_tuple(DecodedListCoordinates)}, {Value, Coordinates}),

                    DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                    ?assertEqual({Value, DecodedLegacyTupleCoordinates}, {Value, Coordinates}),


                    DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, 5, 0, 7),
                    ?assertEqual({Value, DirectDecodedTupleCoordinates}, {Value, Coordinates}),

                    DirectDecodedListCoordinates = erlzord:decode_list(Value, 5, 0, 7),
                    ?assertEqual({Value, list_to_tuple(DirectDecodedListCoordinates)}, {Value, Coordinates}),

                    DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, 5, 0, 7),
                    ?assertEqual({Value, DirectDecodedLegacyTupleCoordinates}, {Value, Coordinates})
                end,
                Terms)
        end
    }.

-endif.