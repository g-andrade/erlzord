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

-export([config/3]).        -ignore_xref({config,3}).
-export([encode/2]).        -ignore_xref({encode,2}).
-export([encode/4]).        -ignore_xref({encode,4}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(IS_UNSIGNED_INT(V), (is_integer((V) andalso (V) >= 0))).

-define(IS_VALID_RANGE(Min, Max), (is_integer((Min)) andalso
                                   is_integer((Max)) andalso
                                   Max >= Min)).

-define(IS_OF_DIMENSION(Coordinates, Dimension),
        ((is_list((Coordinates)) andalso (length((Coordinates)) =:= (Dimension)))
            orelse
         (is_tuple((Coordinates)) andalso (size((Coordinates)) =:= (Dimension))))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-ifdef(pre19).
-opaque config() :: #{ dimension => non_neg_integer(),
                       min_coordinate_value => integer(),
                       max_coordinate_value => integer(),
                       coordinate_bitsize => non_neg_integer() }.
-else.
-opaque config() :: #{ dimension := non_neg_integer(),
                       min_coordinate_value := integer(),
                       max_coordinate_value := integer(),
                       coordinate_bitsize := non_neg_integer() }.
-endif.

-type coordinates() :: [integer()] | tuple().

-export_type([config/0]).
-export_type([coordinates/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec config(Dimension, MinCoordinateValue, MaxCoordinateValue) -> Config
        when Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Config :: config().
config(Dimension, MinCoordinateValue, MaxCoordinateValue)
  when ?IS_UNSIGNED_INT(Dimension),
       ?IS_VALID_RANGE(MinCoordinateValue, MaxCoordinateValue) ->
    Range = MaxCoordinateValue - MinCoordinateValue,
    CoordinateBitsize = unsigned_integer_bitsize(Range),
    #{ dimension => Dimension,
       min_coordinate_value => MinCoordinateValue,
       max_coordinate_value => MaxCoordinateValue,
       coordinate_bitsize => CoordinateBitsize }.

-spec encode(Coordinates, Config) -> Z
        when Coordinates :: coordinates(),
             Config :: config(),
             Z :: non_neg_integer().
encode(Coordinates, #{ dimension := Dimension } = Config)
  when ?IS_OF_DIMENSION(Coordinates, Dimension) ->
    #{ min_coordinate_value := MinCoordinateValue,
       max_coordinate_value := MaxCoordinateValue,
       coordinate_bitsize := CoordinateBitsize } = Config,
    encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).

-spec encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue) -> Z
        when Coordinates :: coordinates(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Z :: non_neg_integer().
encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue)
  when ?IS_OF_DIMENSION(Coordinates, Dimension),
       ?IS_UNSIGNED_INT(Dimension),
       ?IS_VALID_RANGE(MinCoordinateValue, MaxCoordinateValue) ->
    Range = MaxCoordinateValue - MinCoordinateValue,
    CoordinateBitsize = unsigned_integer_bitsize(Range),
    encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize) -> Z
        when Coordinates :: coordinates(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             CoordinateBitsize :: non_neg_integer(),
             Z :: non_neg_integer().
encode(CoordinatesTuple, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize)
  when is_tuple(CoordinatesTuple) ->
    Coordinates = tuple_to_list(CoordinatesTuple),
    encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize);
encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize) ->
    % Cull values and make them unsigned
    NormalizedCoordinates =
        [cull(V, MinCoordinateValue, MaxCoordinateValue) - MinCoordinateValue
         || V <- Coordinates],
    interleave(NormalizedCoordinates, Dimension, CoordinateBitsize).

-spec interleave(Values, Dimension, Bitsize) -> Interleaved
        when Values :: [non_neg_integer()],
             Dimension :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             Interleaved :: non_neg_integer().
interleave(Values, Dimension, Bitsize) ->
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
