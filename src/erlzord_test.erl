-module(erlzord_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.


-spec '10dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'10dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(10, 0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/10dim_from_0_to_100.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 10, 0, 100),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 10, 0, 100),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '10dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'10dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(10, 0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/10dim_from_0_to_12345679.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 10, 0, 12345679),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 10, 0, 12345679),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '10dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'10dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(10, 0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/10dim_from_0_to_7.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 10, 0, 7),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 10, 0, 7),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '1dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'1dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(1, 0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/1dim_from_0_to_100.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 1, 0, 100),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 1, 0, 100),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '1dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'1dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(1, 0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/1dim_from_0_to_12345679.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 1, 0, 12345679),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 1, 0, 12345679),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '1dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'1dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(1, 0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/1dim_from_0_to_7.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 1, 0, 7),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 1, 0, 7),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '2dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'2dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(2, 0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/2dim_from_0_to_100.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 2, 0, 100),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 2, 0, 100),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '2dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'2dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(2, 0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/2dim_from_0_to_12345679.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 2, 0, 12345679),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 2, 0, 12345679),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '2dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'2dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(2, 0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/2dim_from_0_to_7.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 2, 0, 7),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 2, 0, 7),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '30dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'30dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(30, 0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/30dim_from_0_to_100.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 30, 0, 100),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 30, 0, 100),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '30dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'30dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(30, 0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/30dim_from_0_to_12345679.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 30, 0, 12345679),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 30, 0, 12345679),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '30dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'30dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(30, 0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/30dim_from_0_to_7.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 30, 0, 7),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 30, 0, 7),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '3dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'3dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(3, 0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/3dim_from_0_to_100.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 3, 0, 100),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 3, 0, 100),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '3dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'3dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(3, 0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/3dim_from_0_to_12345679.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 3, 0, 12345679),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 3, 0, 12345679),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '3dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'3dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(3, 0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/3dim_from_0_to_7.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 3, 0, 7),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 3, 0, 7),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '4dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'4dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(4, 0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/4dim_from_0_to_100.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 4, 0, 100),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 4, 0, 100),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '4dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'4dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(4, 0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/4dim_from_0_to_12345679.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 4, 0, 12345679),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 4, 0, 12345679),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '4dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'4dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(4, 0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/4dim_from_0_to_7.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 4, 0, 7),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 4, 0, 7),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '5dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'5dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(5, 0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/5dim_from_0_to_100.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 5, 0, 100),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 5, 0, 100),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '5dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'5dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(5, 0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/5dim_from_0_to_12345679.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 5, 0, 12345679),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 5, 0, 12345679),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-spec '5dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'5dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(5, 0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/5dim_from_0_to_7.data"),

        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value}),

                DirectValue = erlzord:encode(Coordinates, 5, 0, 7),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, DirectValue}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({Value, DecodedCoordinates}, {Value, Coordinates}),

                DirectDecodedCoordinates = erlzord:decode(Value, 5, 0, 7),
                ?assertEqual({Value, DirectDecodedCoordinates}, {Value, Coordinates})
            end,
            Terms)
    end.

-endif.