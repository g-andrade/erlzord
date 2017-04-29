-module(erlzord).
-author('Guilherme Andrade <erlzord(at)gandrade(dot)net>').

-ifdef(COMPILE_NATIVE_ERLZORD).
-compile([inline, inline_list_funcs, native, {hipe, o3}]).
-else.
-compile([inline, inline_list_funcs]).
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
    CoordinateBitsize = unsigned_integer_bitsize(Range),
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
    CoordinateBitsize = unsigned_integer_bitsize(Range),
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
    if Value > Max -> Max;
       Value < Min -> Min;
       true -> Value
    end.

-spec unsigned_integer_bitsize(Value) -> Bitsize
        when Value :: non_neg_integer(),
             Bitsize :: non_neg_integer().
unsigned_integer_bitsize(Value) ->
    unsigned_integer_bitsize_recur(Value, 0).

-spec unsigned_integer_bitsize_recur(Value, Acc) -> Bitsize
        when Value :: non_neg_integer(),
             Acc :: non_neg_integer(),
             Bitsize :: non_neg_integer().
unsigned_integer_bitsize_recur(Value, Acc) when Value < 1 ->
    Acc;
unsigned_integer_bitsize_recur(Value, Acc) ->
    unsigned_integer_bitsize_recur(Value bsr 1, Acc + 1).
