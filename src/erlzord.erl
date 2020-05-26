-module(erlzord).
-author('Guilherme Andrade <erlzord(at)gandrade(dot)net>').

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([config/3]).        -ignore_xref({config,3}).
-export([encode/2]).        -ignore_xref({encode,2}).
-export([decode/2]).        -ignore_xref({decode,2}).

-export([encode/4]).        -ignore_xref({encode,4}).
-export([decode/4]).        -ignore_xref({decode,4}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(IS_UNSIGNED_INT(V), (is_integer((V)) andalso (V) >= 0)).

-define(IS_VALID_RANGE(Min, Max), (is_integer((Min)) andalso
                                   is_integer((Max)) andalso
                                   Max >= Min)).

-define(IS_OF_DIMENSION(Coordinates, Dimension),
        ((is_list((Coordinates)) andalso (length((Coordinates)) =:= (Dimension)))
            orelse
         (is_tuple((Coordinates)) andalso (size((Coordinates)) =:= (Dimension))))).

-define(IS_OF_BITSIZE(Value, Bitsize),
        (?IS_UNSIGNED_INT((Value)) andalso
         (Value < (1 bsl (Bitsize))))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-opaque config() :: #{ dimension := non_neg_integer(),
                       min_coordinate_value := integer(),
                       max_coordinate_value := integer(),
                       coordinate_bitsize := non_neg_integer() }.

-type list_coordinates() :: [integer()].
-type tuple_coordinates() :: tuple().
-type coordinates() :: list_coordinates() | tuple_coordinates().

-export_type([config/0]).
-export_type([coordinates/0]).
-export_type([tuple_coordinates/0]).
-export_type([list_coordinates/0]).

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
    encode_(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).


-spec decode(Z, Config) -> Coordinates
        when Z :: non_neg_integer(),
             Config :: config(),
             Coordinates :: tuple_coordinates().

decode(Z, #{ dimension := Dimension, coordinate_bitsize := CoordinateBitsize } = Config)
  when ?IS_OF_BITSIZE(Z, Dimension * CoordinateBitsize) ->
    #{ min_coordinate_value := MinCoordinateValue,
       max_coordinate_value := MaxCoordinateValue } = Config,
    decode_(Z, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).


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
    encode_(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).


-spec decode(Z, Dimension, MinCoordinateValue, MaxCoordinateValue) -> Coordinates
        when Z :: non_neg_integer(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Coordinates :: tuple_coordinates().

decode(Z, Dimension, MinCoordinateValue, MaxCoordinateValue)
  when ?IS_UNSIGNED_INT(Z),
       ?IS_UNSIGNED_INT(Dimension),
       ?IS_VALID_RANGE(MinCoordinateValue, MaxCoordinateValue) ->
    Range = MaxCoordinateValue - MinCoordinateValue,
    CoordinateBitsize = unsigned_integer_bitsize(Range),
    (?IS_OF_BITSIZE(Z, Dimension * CoordinateBitsize) orelse exit({badarg,Z})),
    decode_(Z, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec encode_(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize) -> Z
        when Coordinates :: coordinates(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             CoordinateBitsize :: non_neg_integer(),
             Z :: non_neg_integer().

encode_(CoordinatesTuple, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize)
  when is_tuple(CoordinatesTuple) ->
    Coordinates = tuple_to_list(CoordinatesTuple),
    encode_(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize);
encode_(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize) ->
    % Cull values and make them unsigned
    NormalizedCoordinates =
        [cull(V, MinCoordinateValue, MaxCoordinateValue) - MinCoordinateValue
         || V <- Coordinates],
    interleave(NormalizedCoordinates, Dimension, CoordinateBitsize).


-spec interleave(Values, Dimension, Bitsize) -> Interleaved
        when Values :: list_coordinates(),
             Dimension :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             Interleaved :: non_neg_integer().

interleave(Values, Dimension, Bitsize) ->
    interleave_recur(Values, Dimension, Bitsize, Bitsize, 0).


-spec interleave_recur(Values, Dimension, TotalBitsize, BitIndex, Acc) -> Interleaved
        when Values :: list_coordinates(),
             Dimension :: non_neg_integer(),
             TotalBitsize :: non_neg_integer(),
             BitIndex :: non_neg_integer(),
             Acc :: non_neg_integer(),
             Interleaved :: non_neg_integer().

interleave_recur(_Values, _Dimension, _TotalBitsize, 0 = _BitIndex, Acc) ->
    Acc;
interleave_recur(Values, Dimension, TotalBitsize, BitIndex, Acc) ->
    {Conjugated, NewValues} = conjugate_values(Values),
    ShiftAmount = Dimension * (TotalBitsize - BitIndex),
    ShiftedConjugated = Conjugated bsl ShiftAmount,
    NewAcc = Acc bor ShiftedConjugated,
    interleave_recur(NewValues, Dimension, TotalBitsize, BitIndex - 1, NewAcc).


-spec conjugate_values(Values) -> {Conjugated, NewValues}
        when Values :: [non_neg_integer()],
             Conjugated :: non_neg_integer(),
             NewValues :: [non_neg_integer()].

conjugate_values(Values) ->
    conjugate_values_recur(Values, 0, 0, []).


-spec conjugate_values_recur(Values, DimensionIndex, AccConjugated, AccNewValues)
    -> {Conjugated, NewValues}
        when Values :: list_coordinates(),
             DimensionIndex :: non_neg_integer(),
             AccConjugated :: non_neg_integer(),
             AccNewValues :: list_coordinates(),
             Conjugated :: non_neg_integer(),
             NewValues :: list_coordinates().

conjugate_values_recur([], _DimensionIndex, AccConjugated, AccNewValues) ->
    {AccConjugated, lists:reverse(AccNewValues)};
conjugate_values_recur([H | T], DimensionIndex, AccConjugated, AccNewValues) ->
    Bit = (H band 1) bsl DimensionIndex,
    NewH = H bsr 1,
    conjugate_values_recur(T, DimensionIndex + 1, Bit bor AccConjugated, [NewH | AccNewValues]).


-spec decode_(Z, Dimension, MinCoordinateValue, MaxCoordinateValue,
              Bitsize) -> Coordinates
        when Z :: non_neg_integer(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Bitsize :: non_neg_integer(),
             Coordinates :: tuple_coordinates().

decode_(Z, Dimension, MinCoordinateValue, MaxCoordinateValue, Bitsize) ->
    {NewZ, NormalizedCoordinates} = revert_interleave(Z, Dimension, Bitsize),

    (NewZ =:= 0 orelse throw({badarg,Z})), % some higher bits weren't processed
    Coordinates =
        lists:map(
          fun (NormalizedValue) ->
                  Value = MinCoordinateValue + NormalizedValue,
                  (Value =< MaxCoordinateValue orelse throw({badarg, Z})), % out of range
                  Value
          end,
          NormalizedCoordinates),

    list_to_tuple(Coordinates).


-spec revert_interleave(Interleaved, Dimension, Bitsize) -> {NewInterLeaved, Values}
        when Interleaved :: non_neg_integer(),
             Dimension :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             NewInterLeaved :: non_neg_integer(),
             Values :: [non_neg_integer()].

revert_interleave(Interleaved, Dimension, Bitsize) ->
    Acc0 = repeat(0, Dimension),
    revert_interleave_recur(Interleaved, Dimension, Bitsize, 0, Acc0).


-spec revert_interleave_recur(Interleaved, Dimension, Bitsize, BitIndex,
                              Acc) -> {NewInterLeaved, FinalAcc}
        when Interleaved :: non_neg_integer(),
             Dimension :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             BitIndex :: non_neg_integer(),
             Acc :: [non_neg_integer()],
             NewInterLeaved :: non_neg_integer(),
             FinalAcc :: [non_neg_integer()].

revert_interleave_recur(Interleaved, _Dimension, Bitsize, BitIndex, Acc)
  when BitIndex >= Bitsize ->
    {Interleaved, Acc};
revert_interleave_recur(Interleaved, Dimension, Bitsize, BitIndex, Acc) ->
    {NewInterLeaved, NewAcc} = unchain_values(Interleaved, Bitsize, BitIndex, Acc),
    revert_interleave_recur(NewInterLeaved, Dimension, Bitsize,
                            BitIndex + 1, NewAcc).


-spec unchain_values(Conjugated, Bitsize, BitIndex,
                     ValuesAcc) -> {NewConjugated, NewValuesAcc}
        when Conjugated :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             BitIndex :: non_neg_integer(),
             ValuesAcc :: [non_neg_integer()],
             NewConjugated :: non_neg_integer(),
             NewValuesAcc :: [non_neg_integer()].

unchain_values(Conjugated, Bitsize, BitIndex, ValuesAcc) ->
    unchain_values_recur(Conjugated, Bitsize, BitIndex, ValuesAcc, []).


-spec unchain_values_recur(Conjugated, Bitsize, BitIndex,
                           ValuesAcc, NewValuesAcc) -> {NewConjugated, FinalValuesAcc}
        when Conjugated :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             BitIndex :: non_neg_integer(),
             ValuesAcc :: [non_neg_integer()],
             NewConjugated :: non_neg_integer(),
             NewValuesAcc :: [non_neg_integer()],
             FinalValuesAcc :: [non_neg_integer()].

unchain_values_recur(Conjugated, _Bitsize, _BitIndex, [], NewValuesAcc) ->
    {Conjugated, lists:reverse(NewValuesAcc)};
unchain_values_recur(Conjugated, Bitsize, BitIndex, [H|T], NewValuesAcc) ->
    Bit = Conjugated band 1,
    NewConjugated = Conjugated bsr 1,
    NewH = H bor (Bit bsl BitIndex),
    unchain_values_recur(NewConjugated, Bitsize, BitIndex, T, [NewH | NewValuesAcc]).


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


-spec repeat(V, N) -> FinalAcc
        when V :: ValueT,
             N :: non_neg_integer(),
             FinalAcc :: [ValueT, ...],
             ValueT :: 0. % sigh

repeat(V, N) ->
    repeat_recur(V, N, []).


-spec repeat_recur(V, N, Acc) -> FinalAcc
        when V :: ValueT,
             N :: non_neg_integer(),
             Acc :: [ValueT],
             FinalAcc :: [ValueT, ...],
             ValueT :: 0.

repeat_recur(_V, N, Acc) when N < 1->
    Acc;
repeat_recur(V, N, Acc) ->
    repeat_recur(V, N - 1, [V | Acc]).
