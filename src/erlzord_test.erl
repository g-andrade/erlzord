-module(erlzord_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.


-spec '10dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'10dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/10dim_from_0_to_100.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '10dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'10dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/10dim_from_0_to_12345679.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '10dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'10dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/10dim_from_0_to_7.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '1dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'1dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/1dim_from_0_to_100.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '1dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'1dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/1dim_from_0_to_12345679.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '1dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'1dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/1dim_from_0_to_7.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '2dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'2dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/2dim_from_0_to_100.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '2dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'2dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/2dim_from_0_to_12345679.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '2dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'2dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/2dim_from_0_to_7.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '30dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'30dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/30dim_from_0_to_100.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '30dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'30dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/30dim_from_0_to_12345679.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '30dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'30dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/30dim_from_0_to_7.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '3dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'3dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/3dim_from_0_to_100.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '3dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'3dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/3dim_from_0_to_12345679.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '3dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'3dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/3dim_from_0_to_7.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '4dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'4dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/4dim_from_0_to_100.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '4dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'4dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/4dim_from_0_to_12345679.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '4dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'4dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/4dim_from_0_to_7.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '5dim_from_0_to_100.data_test_'() -> fun(() -> ok).
'5dim_from_0_to_100.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 100),
        {ok, Terms} = file:consult("test_cases/test_data/5dim_from_0_to_100.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '5dim_from_0_to_12345679.data_test_'() -> fun(() -> ok).
'5dim_from_0_to_12345679.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 12345679),
        {ok, Terms} = file:consult("test_cases/test_data/5dim_from_0_to_12345679.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-spec '5dim_from_0_to_7.data_test_'() -> fun(() -> ok).
'5dim_from_0_to_7.data_test_'() ->
    fun () ->
        Config = erlzord:config(0, 7),
        {ok, Terms} = file:consult("test_cases/test_data/5dim_from_0_to_7.data"),
        
        lists:foreach(
            fun ({Coordinates, ExpectedValue}) ->
                Value = erlzord:calculate(Coordinates, Config),
                ?assertEqual({Coordinates, ExpectedValue}, {Coordinates, Value})
            end,
            Terms)
    end.

-endif.