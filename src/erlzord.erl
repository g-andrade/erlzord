-module(erlzord).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([config/1,
         encode/2,
         decode_tuple/2,
         decode_list/2,
         encode/3,
         decode_tuple/4,
         decode_list/4,
         % Deprecated exports
         config/3,
         decode/2,
         encode/4,
         decode/4]).

-ignore_xref(
        [config/1,
         encode/2,
         decode_tuple/2,
         decode_list/2,
         encode/3,
         decode_tuple/4,
         decode_list/4,
         % Deprecated exports
         config/3,
         decode/2,
         encode/4,
         decode/4]).

-deprecated({config, 3, "Use config/1 instead"}).
-deprecated({decode, 2, "Use decode_tuple/2 or decode_list/2 instead"}).
-deprecated({encode, 4, "Use encode/3 instead"}).
-deprecated({decode, 4, "Use decode_tuple/4 or decode_list/4 instead"}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(IS_UNSIGNED_INT(V), (is_integer((V)) andalso (V) >= 0)).

-define(IS_OF_BITSIZE(Value, Bitsize),
        (?IS_UNSIGNED_INT((Value)) andalso
         (Value < (1 bsl (Bitsize))))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type config_params() :: #{
    (dimensions | dimension) := non_neg_integer(),
    min_coordinate_value := integer(),
    max_coordinate_value := integer()
}.

-export_type([config_params/0]).

-record(erlzord_config, {
    dimension :: non_neg_integer(),
    min_coordinate_value :: integer(),
    max_coordinate_value :: integer(),
    coordinate_bitsize :: non_neg_integer()
}).
-opaque config() :: #erlzord_config{}.

-export_type([config/0]).

-type tuple_coordinates() :: tuple().
-type list_coordinates() :: [integer()].
-type coordinates() :: tuple_coordinates() | list_coordinates().

-export_type([coordinates/0]).
-export_type([tuple_coordinates/0]).
-export_type([list_coordinates/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec config(Params) -> Config
        when Params :: config_params(),
             Config :: config().
config(ConfigParams) ->
    case ConfigParams of
        #{ dimensions := Dimension,
           min_coordinate_value := MinCoordinateValue,
           max_coordinate_value := MaxCoordinateValue
         } when map_size(ConfigParams) =:= 3 ->
            config_(Dimension, MinCoordinateValue, MaxCoordinateValue);

        #{ dimension := Dimension,
           min_coordinate_value := MinCoordinateValue,
           max_coordinate_value := MaxCoordinateValue
         } when map_size(ConfigParams) =:= 3 ->
            config_(Dimension, MinCoordinateValue, MaxCoordinateValue);

        _ ->
            error({badarg, ConfigParams})
    end.

-spec encode(Coordinates, Config) -> Z
        when Coordinates :: coordinates(),
             Config :: config(),
             Z :: non_neg_integer().
encode(Coordinates, #erlzord_config{
                       dimension = Dimension,
                       min_coordinate_value = MinCoordinateValue,
                       max_coordinate_value = MaxCoordinateValue,
                       coordinate_bitsize = CoordinateBitsize
       }) ->

    case Coordinates of
        Tuple when tuple_size(Tuple) =:= Dimension ->
            List = tuple_to_list(Tuple),
            encode_list_(List, Dimension, MinCoordinateValue,
                         MaxCoordinateValue, CoordinateBitsize);

        List when length(List) =:= Dimension ->
            encode_list_(List, Dimension, MinCoordinateValue,
                         MaxCoordinateValue, CoordinateBitsize);

        BadArg ->
            error({badarg, BadArg})
    end;
encode(_Coordinates, BadArg) ->
    error({badarg, BadArg}).

-spec decode_tuple(Z, Config) -> Coordinates
        when Z :: non_neg_integer(),
             Config :: config(),
             Coordinates :: tuple_coordinates().
decode_tuple(Z, Config) ->
    List = decode_list(Z, Config),
    list_to_tuple(List).

-spec decode_list(Z, Config) -> Coordinates
        when Z :: non_neg_integer(),
             Config :: config(),
             Coordinates :: list_coordinates().
decode_list(Z, #erlzord_config{
                  dimension = Dimension,
                  min_coordinate_value = MinCoordinateValue,
                  max_coordinate_value = MaxCoordinateValue,
                  coordinate_bitsize = CoordinateBitsize
    }) ->
    validate_z(Z, Dimension, CoordinateBitsize),
    decode_list_(Z, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize);
decode_list(_Z, BadArg) ->
    error({badarg, BadArg}).

-spec encode(Coordinates, MinCoordinateValue, MaxCoordinateValue) -> Z
        when Coordinates :: coordinates(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Z :: non_neg_integer().
encode(Coordinates, MinCoordinateValue, MaxCoordinateValue) ->
    {Dimension, List} =
        case Coordinates of
            TupleCoords when is_tuple(TupleCoords) ->
                {tuple_size(TupleCoords), tuple_to_list(TupleCoords)};

            ListCoords when is_list(ListCoords) ->
                {length(ListCoords), ListCoords};

            _ ->
                error({badarg, Coordinates})
        end,

    validate_params(Dimension, MinCoordinateValue, MaxCoordinateValue),
    CoordinateBitsize = coordinate_bitsize(MinCoordinateValue, MaxCoordinateValue),
    encode_list_(List, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).

-spec decode_tuple(Z, Dimension, MinCoordinateValue, MaxCoordinateValue) -> Coordinates
        when Z :: non_neg_integer(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Coordinates :: tuple_coordinates().
decode_tuple(Z, Dimension, MinCoordinateValue, MaxCoordinateValue) ->
    List = decode_list(Z, Dimension, MinCoordinateValue, MaxCoordinateValue),
    list_to_tuple(List).

-spec decode_list(Z, Dimension, MinCoordinateValue, MaxCoordinateValue) -> Coordinates
        when Z :: non_neg_integer(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Coordinates :: list_coordinates().
decode_list(Z, Dimension, MinCoordinateValue, MaxCoordinateValue) ->
    validate_params(Dimension, MinCoordinateValue, MaxCoordinateValue),

    CoordinateBitsize = coordinate_bitsize(MinCoordinateValue, MaxCoordinateValue),
    validate_z(Z, Dimension, CoordinateBitsize),

    decode_list_(Z, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize).

%% ------------------------------------------------------------------
%% Deprecated API Function Definitions
%% ------------------------------------------------------------------

-spec config(Dimension, MinCoordinateValue, MaxCoordinateValue) -> Config
        when Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Config :: config().
config(Dimension, MinCoordinateValue, MaxCoordinateValue) ->
    config_(Dimension, MinCoordinateValue, MaxCoordinateValue).

-spec decode(Z, Config) -> Coordinates
        when Z :: non_neg_integer(),
             Config :: config(),
             Coordinates :: tuple_coordinates().
decode(Z, Config) ->
    decode_tuple(Z, Config).

-spec encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue) -> Z
        when Coordinates :: coordinates(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Z :: non_neg_integer().
encode(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue)
  when (
    (is_tuple(Coordinates) andalso (tuple_size(Coordinates) =:= Dimension))
    orelse
    (length(Coordinates) =:= Dimension)
  ) ->
    encode(Coordinates, MinCoordinateValue, MaxCoordinateValue).

-spec decode(Z, Dimension, MinCoordinateValue, MaxCoordinateValue) -> Coordinates
        when Z :: non_neg_integer(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Coordinates :: tuple_coordinates().
decode(Z, Dimension, MinCoordinateValue, MaxCoordinateValue) ->
    decode_tuple(Z, Dimension, MinCoordinateValue, MaxCoordinateValue).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec config_(Dimension, MinCoordinateValue, MaxCoordinateValue) -> Config
        when Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Config :: config().
%% @doc Returns a pre-validated config which `:encode/2' and `:decode/2'` can use.
config_(Dimension, MinCoordinateValue, MaxCoordinateValue) ->
    validate_params(Dimension, MinCoordinateValue, MaxCoordinateValue),

    #erlzord_config{
       dimension = Dimension,
       min_coordinate_value = MinCoordinateValue,
       max_coordinate_value = MaxCoordinateValue,
       coordinate_bitsize = coordinate_bitsize(MinCoordinateValue, MaxCoordinateValue)
    }.

validate_params(Dimension, MinCoordinateValue, MaxCoordinateValue) ->
    case '_' of
        _ when not ?IS_UNSIGNED_INT(Dimension) ->
            error({badarg, Dimension});

        _ when not is_integer(MinCoordinateValue) ->
            error({badarg, MinCoordinateValue});

        _ when not is_integer(MaxCoordinateValue) ->
            error({badarg, MaxCoordinateValue});

        _ when MaxCoordinateValue < MinCoordinateValue ->
            error({badarg, MaxCoordinateValue});

        _ ->
            ok
    end.

validate_z(Z, Dimension, CoordinateBitsize) ->
    case ?IS_OF_BITSIZE(Z, Dimension * CoordinateBitsize) of
        true ->
            ok;

        false ->
            error({badarg, Z})
    end.

coordinate_bitsize(MinCoordinateValue, MaxCoordinateValue) ->
    Range = MaxCoordinateValue - MinCoordinateValue,
    unsigned_integer_bitsize(Range).

-spec encode_list_(Coordinates, Dimension, MinCoordinateValue,
              MaxCoordinateValue, CoordinateBitsize) -> Z
        when Coordinates :: list_coordinates(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             CoordinateBitsize :: non_neg_integer(),
             Z :: non_neg_integer().
%% @doc Encodes `Coordinates', a tuple or list of `Dimension`
%% Returns a pre-validated config which `:encode/2' and `:decode/2'` can use.
encode_list_(Coordinates, Dimension, MinCoordinateValue, MaxCoordinateValue, CoordinateBitsize) ->
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

-spec decode_list_(Z, Dimension, MinCoordinateValue, MaxCoordinateValue,
                   Bitsize) -> Coordinates
        when Z :: non_neg_integer(),
             Dimension :: non_neg_integer(),
             MinCoordinateValue :: integer(),
             MaxCoordinateValue :: integer(),
             Bitsize :: non_neg_integer(),
             Coordinates :: list_coordinates().
decode_list_(Z, Dimension, MinCoordinateValue, MaxCoordinateValue, Bitsize) ->
    {NewZ, NormalizedCoordinates} = revert_interleave(Z, Dimension, Bitsize),

    (NewZ =:= 0 orelse error({badarg, Z})), % some higher bits weren't processed
    lists:map(
      fun (NormalizedValue) ->
              Value = MinCoordinateValue + NormalizedValue,
              (Value =< MaxCoordinateValue orelse error({badarg, Z})), % out of range
              Value
      end,
      NormalizedCoordinates).

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
unchain_values_recur(Conjugated, Bitsize, BitIndex, [H | T], NewValuesAcc) ->
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
    case Value of
        _ when Value > Max ->
            Max;
        _ when Value < Min ->
            Min;
        _ ->
            Value
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
repeat_recur(_V, N, Acc) when N < 1 ->
    Acc;
repeat_recur(V, N, Acc) ->
    repeat_recur(V, N - 1, [V | Acc]).
