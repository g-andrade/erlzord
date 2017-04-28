-module(erlzord).
-author('Guilherme Andrade <erlzord(at)gandrade(dot)net>').

-ifdef(COMPILE_NATIVE_ERLZORD).
-compile([inline, inline_list_funcs, native, {hipe, o3}]).
-else.
-compile([inline, inline_list_funcs]).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([config/2]).        -ignore_xref({config,2}).
-export([calculate/2]).     -ignore_xref({calculate,2}).
-export([calculate/3]).     -ignore_xref({calculate,3}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(IS_VALID_RANGE(Min, Max), (is_integer((Min)) andalso
                                   is_integer((Max)) andalso
                                   Max >= Min)).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-ifdef(pre19).
-define(MAPT_KV_REQ_SPEC(K, V), (K) => (V)).
-else.
% otherwise Dialyzer will complain of overspec
-define(MAPT_KV_REQ_SPEC(K, V), (K) := (V)).
-endif.

-opaque config() :: #{ ?MAPT_KV_REQ_SPEC(min_coordinate_value, integer()),
                       ?MAPT_KV_REQ_SPEC(max_coordinate_value, integer()),
                       ?MAPT_KV_REQ_SPEC(coordinate_bitsize, non_neg_integer()) }.

-type coordinates() :: [integer()] | tuple().

-export_type([config/0]).
-export_type([coordinates/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec config(MinCoordinateValue, MaxCoordinateValue) -> Config
        when MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Config :: config().
config(MinCoordinateValue, MaxCoordinateValue)
  when ?IS_VALID_RANGE(MinCoordinateValue, MaxCoordinateValue) ->
    Range = MaxCoordinateValue - MinCoordinateValue,
    CoordinateBitsize = positive_integer_bitsize(Range),
    #{ min_coordinate_value => MinCoordinateValue,
       max_coordinate_value => MaxCoordinateValue,
       coordinate_bitsize => CoordinateBitsize }.

-spec calculate(Coordinates, Config) -> Z
        when Coordinates :: coordinates(),
             Config :: config(),
             Z :: non_neg_integer().
calculate(Coordinates, Config) ->
    #{ min_coordinate_value := MinCoordinateValue,
       max_coordinate_value := MaxCoordinateValue,
       coordinate_bitsize := CoordinateBitsize } = Config,
    calculate(Coordinates, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).

-spec calculate(Coordinates, MinCoordinateValue, MaxCoordinateValue) -> Z
        when Coordinates :: coordinates(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Z :: non_neg_integer().
calculate(Coordinates, MinCoordinateValue, MaxCoordinateValue)
  when ?IS_VALID_RANGE(MinCoordinateValue, MaxCoordinateValue) ->
    Range = MaxCoordinateValue - MinCoordinateValue,
    CoordinateBitsize = positive_integer_bitsize(Range),
    calculate(Coordinates, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec calculate(Coordinates, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize) -> Z
        when Coordinates :: coordinates(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             CoordinateBitsize :: non_neg_integer(),
             Z :: non_neg_integer().
calculate(CoordinatesTuple, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize)
  when is_tuple(CoordinatesTuple) ->
    Coordinates = tuple_to_list(CoordinatesTuple),
    calculate(Coordinates, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize);
calculate(Coordinates, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize) ->
    % Make sure they're all between MinCoordinateValue and MaxCoordinateValue
    NormalizedCoordinates =
        [cull(V, MinCoordinateValue, MaxCoordinateValue) - MinCoordinateValue
         || V <- Coordinates],
    interleave(NormalizedCoordinates, CoordinateBitsize).

-spec interleave(Values, Bitsize) -> Interleaved
        when Values :: [non_neg_integer()],
             Bitsize :: non_neg_integer(),
             Interleaved :: non_neg_integer().
interleave(Values, Bitsize) ->
    Dimension = length(Values),
    interleave_recur(Values, Dimension, Bitsize, Bitsize, 0).

-spec interleave_recur(Values, Dimension, TotalBitsize, Bitsize, Acc) -> Interleaved
        when Values :: [non_neg_integer()],
             Dimension :: non_neg_integer(),
             TotalBitsize :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             Acc :: non_neg_integer(),
             Interleaved :: non_neg_integer().
interleave_recur(_Values, _Dimension, _TotalBitsize, 0 = _Bitsize, Acc) ->
    Acc;
interleave_recur(Values, Dimension, TotalBitsize, Bitsize, Acc) ->
    {Conjugated, NewValues} = conjugate_values(Values),
    ShiftAmount = Dimension * (TotalBitsize - Bitsize),
    ShiftedConjugated = Conjugated bsl ShiftAmount,
    NewAcc = Acc bor ShiftedConjugated,
    interleave_recur(NewValues, Dimension, TotalBitsize, Bitsize - 1, NewAcc).

-spec conjugate_values(Values) -> {Conjugated, NewValues}
        when Values :: [non_neg_integer()],
             Conjugated :: non_neg_integer(),
             NewValues :: [non_neg_integer()].
conjugate_values(Values) ->
    conjugate_values_recur(Values, 0, 0, []).

-spec conjugate_values_recur(Values, DimensionIndex, AccConjugated, AccNewValues)
    -> {Conjugated, NewValues}
        when Values :: [non_neg_integer()],
             DimensionIndex :: non_neg_integer(),
             AccConjugated :: non_neg_integer(),
             AccNewValues :: [non_neg_integer()],
             Conjugated :: non_neg_integer(),
             NewValues :: [non_neg_integer()].
conjugate_values_recur([], _DimensionIndex, AccConjugated, AccNewValues) ->
    {AccConjugated, lists:reverse(AccNewValues)};
conjugate_values_recur([H | T], DimensionIndex, AccConjugated, AccNewValues) ->
    Bit = (H band 1) bsl DimensionIndex,
    NewH = H bsr 1,
    conjugate_values_recur(T, DimensionIndex + 1, Bit bor AccConjugated, [NewH | AccNewValues]).

-spec cull(Value, Min, Max) -> CulledValue
        when Value :: integer(),
             Min :: integer(),
             Max :: integer(),
             CulledValue :: integer().
cull(Value, Min, Max) when is_integer(Value) ->
    max(Min, min(Max, Value)).

-spec positive_integer_bitsize(Value) -> Log2
        when Value :: pos_integer(),
             Log2 :: non_neg_integer().
positive_integer_bitsize(Value) when Value =< 1 bsl 1023 ->
    ceil(log2(Value + 1));
positive_integer_bitsize(Value) ->
    1023 + positive_integer_bitsize(Value div (1 bsl 1023)).

-spec ceil(Value) -> Ceil
        when Value :: float(),
             Ceil :: integer().
ceil(Value) ->
    TruncValue = trunc(Value),
    case TruncValue == Value of
        true -> TruncValue;
        false -> TruncValue + 1
    end.

-spec log2(Value) -> Log2
        when Value :: pos_integer(),
             Log2 :: float().
-ifdef(pre18).
log2(V) -> math:log(V) / math:log(2).
-else.
log2(V) -> math:log2(V).
-endif.

%% ------------------------------------------------------------------
%% Unit Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

-spec test() -> ok.

-spec basic_test_() -> fun(() -> ok).
basic_test_() ->
    fun () ->
            Config = config(0, 7),
            Coordinates =
                [{X, Y} || X <- lists:seq(0, 7), Y <- lists:seq(0, 7)],

            ExpectedValues =
                [0,2,8,10,32,34,40,42,1,3,9,11,33,35,41,43,4,6,12,14,36,38,
                 44,46,5,7,13,15,37,39,45,47,16,18,24,26,48,50,56,58,17,19,
                 25,27,49,51,57,59,20,22,28,30,52,54,60,62,21,23,29,31,53,55,
                 61,63],

            lists_foreach2(
              fun ({X, Y}, ExpectedValue) ->
                      Value = calculate([X, Y], Config),
                      ?assertEqual(Value, ExpectedValue)
              end,
              Coordinates,
              ExpectedValues)
    end.

-spec lists_foreach2(Fun, L1, L2) -> ok
        when Fun :: fun ((T1, T2) -> ok),
             L1 :: [T1],
             L2 :: [T2].
lists_foreach2(_Fun, [], []) ->
    ok;
lists_foreach2(Fun, [H1|T1], [H2|T2]) ->
    Fun(H1, H2),
    lists_foreach2(Fun, T1, T2).

-endif.
