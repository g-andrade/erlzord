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

-ifndef(OTP_RELEASE).
-define(NO_XREF_DEPRECATED_WITH_MSG, true).
-else.
-if(?OTP_RELEASE =< 22).
-define(NO_XREF_DEPRECATED_WITH_MSG, true).
-endif.
-endif.

-ifdef(NO_XREF_DEPRECATED_WITH_MSG).
-deprecated([
   {config, 3},
   {decode, 2},
   {encode, 4},
   {decode, 4}
]).
-else.
-deprecated([
   {config, 3, "Use config/1 instead"},
   {decode, 2, "Use decode_tuple/2 or decode_list/2 instead"},
   {encode, 4, "Use encode/3 instead"},
   {decode, 4, "Use decode_tuple/4 or decode_list/4 instead"}
]).
-endif.

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
    dimensions := non_neg_integer(),
    min_component := integer(),
    max_component := integer()
}.

-export_type([config_params/0]).

-record(erlzord_config, {
    dimensions :: non_neg_integer(),
    min_component :: integer(),
    max_component :: integer(),
    component_bitsize :: non_neg_integer()
}).
-opaque config() :: #erlzord_config{}.

-export_type([config/0]).

-type tuple_vector() :: tuple().
-type list_vector() :: [integer()].
-type vector() :: tuple_vector() | list_vector().

-export_type([vector/0]).
-export_type([tuple_vector/0]).
-export_type([list_vector/0]).

%% ------------------------------------------------------------------
%% Deprecated Type Definitions
%% ------------------------------------------------------------------

-type tuple_coordinates() :: tuple_vector().
-type list_coordinates() :: list_vector().
-type coordinates() :: vector().

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
        #{ dimensions := Dimensions,
           min_component := MinComponent,
           max_component := MaxComponent
         } when map_size(ConfigParams) =:= 3 ->
            config_(Dimensions, MinComponent, MaxComponent);

        _ ->
            error({badarg, ConfigParams})
    end.

-spec encode(Vector, Config) -> Z
        when Vector :: vector(),
             Config :: config(),
             Z :: non_neg_integer().
encode(Vector, #erlzord_config{
                       dimensions = Dimensions,
                       min_component = MinComponent,
                       max_component = MaxComponent,
                       component_bitsize = ComponentBitsize
       }) ->

    case Vector of
        Tuple when tuple_size(Tuple) =:= Dimensions ->
            List = tuple_to_list(Tuple),
            encode_list_(List, Dimensions, MinComponent,
                         MaxComponent, ComponentBitsize);

        List when length(List) =:= Dimensions ->
            encode_list_(List, Dimensions, MinComponent,
                         MaxComponent, ComponentBitsize);

        BadArg ->
            error({badarg, BadArg})
    end;
encode(_Vector, BadArg) ->
    error({badarg, BadArg}).

-spec decode_tuple(Z, Config) -> Vector
        when Z :: non_neg_integer(),
             Config :: config(),
             Vector :: tuple_vector().
decode_tuple(Z, Config) ->
    List = decode_list(Z, Config),
    list_to_tuple(List).

-spec decode_list(Z, Config) -> Vector
        when Z :: non_neg_integer(),
             Config :: config(),
             Vector :: list_vector().
decode_list(Z, #erlzord_config{
                  dimensions = Dimensions,
                  min_component = MinComponent,
                  max_component = MaxComponent,
                  component_bitsize = ComponentBitsize
    }) ->
    validate_z(Z, Dimensions, ComponentBitsize),
    decode_list_(Z, Dimensions, MinComponent, MaxComponent, ComponentBitsize);
decode_list(_Z, BadArg) ->
    error({badarg, BadArg}).

-spec encode(Vector, MinComponent, MaxComponent) -> Z
        when Vector :: vector(),
             MinComponent :: integer(),
             MaxComponent :: integer(),
             Z :: non_neg_integer().
encode(Vector, MinComponent, MaxComponent) ->
    {Dimensions, List} =
        case Vector of
            TupleCoords when is_tuple(TupleCoords) ->
                {tuple_size(TupleCoords), tuple_to_list(TupleCoords)};

            ListCoords when is_list(ListCoords) ->
                {length(ListCoords), ListCoords};

            _ ->
                error({badarg, Vector})
        end,

    validate_params(Dimensions, MinComponent, MaxComponent),
    ComponentBitsize = component_bitsize(MinComponent, MaxComponent),
    encode_list_(List, Dimensions, MinComponent, MaxComponent, ComponentBitsize).

-spec decode_tuple(Z, Dimensions, MinComponent, MaxComponent) -> Vector
        when Z :: non_neg_integer(),
             Dimensions :: non_neg_integer(),
             MinComponent :: integer(),
             MaxComponent :: integer(),
             Vector :: tuple_vector().
decode_tuple(Z, Dimensions, MinComponent, MaxComponent) ->
    List = decode_list(Z, Dimensions, MinComponent, MaxComponent),
    list_to_tuple(List).

-spec decode_list(Z, Dimensions, MinComponent, MaxComponent) -> Vector
        when Z :: non_neg_integer(),
             Dimensions :: non_neg_integer(),
             MinComponent :: integer(),
             MaxComponent :: integer(),
             Vector :: list_vector().
decode_list(Z, Dimensions, MinComponent, MaxComponent) ->
    validate_params(Dimensions, MinComponent, MaxComponent),

    ComponentBitsize = component_bitsize(MinComponent, MaxComponent),
    validate_z(Z, Dimensions, ComponentBitsize),

    decode_list_(Z, Dimensions, MinComponent, MaxComponent, ComponentBitsize).

%% ------------------------------------------------------------------
%% Deprecated API Function Definitions
%% ------------------------------------------------------------------

-spec config(Dimensions, MinComponent, MaxComponent) -> Config
        when Dimensions :: non_neg_integer(),
             MinComponent :: integer(),
             MaxComponent :: integer(),
             Config :: config().
config(Dimensions, MinComponent, MaxComponent) ->
    config_(Dimensions, MinComponent, MaxComponent).

-spec decode(Z, Config) -> Vector
        when Z :: non_neg_integer(),
             Config :: config(),
             Vector :: tuple_vector().
decode(Z, Config) ->
    decode_tuple(Z, Config).

-spec encode(Vector, Dimensions, MinComponent, MaxComponent) -> Z
        when Vector :: vector(),
             Dimensions :: non_neg_integer(),
             MinComponent :: integer(),
             MaxComponent :: integer(),
             Z :: non_neg_integer().
encode(Vector, Dimensions, MinComponent, MaxComponent)
  when (
    (is_tuple(Vector) andalso (tuple_size(Vector) =:= Dimensions))
    orelse
    (length(Vector) =:= Dimensions)
  ) ->
    encode(Vector, MinComponent, MaxComponent).

-spec decode(Z, Dimensions, MinComponent, MaxComponent) -> Vector
        when Z :: non_neg_integer(),
             Dimensions :: non_neg_integer(),
             MinComponent :: integer(),
             MaxComponent :: integer(),
             Vector :: tuple_vector().
decode(Z, Dimensions, MinComponent, MaxComponent) ->
    decode_tuple(Z, Dimensions, MinComponent, MaxComponent).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec config_(Dimensions, MinComponent, MaxComponent) -> Config
        when Dimensions :: non_neg_integer(),
             MinComponent :: integer(),
             MaxComponent :: integer(),
             Config :: config().
%% @doc Returns a pre-validated config which `:encode/2' and `:decode/2'` can use.
config_(Dimensions, MinComponent, MaxComponent) ->
    validate_params(Dimensions, MinComponent, MaxComponent),

    #erlzord_config{
       dimensions = Dimensions,
       min_component = MinComponent,
       max_component = MaxComponent,
       component_bitsize = component_bitsize(MinComponent, MaxComponent)
    }.

validate_params(Dimensions, MinComponent, MaxComponent) ->
    case '_' of
        _ when not ?IS_UNSIGNED_INT(Dimensions) ->
            error({badarg, Dimensions});

        _ when not is_integer(MinComponent) ->
            error({badarg, MinComponent});

        _ when not is_integer(MaxComponent) ->
            error({badarg, MaxComponent});

        _ when MaxComponent < MinComponent ->
            error({badarg, MaxComponent});

        _ ->
            ok
    end.

validate_z(Z, Dimensions, ComponentBitsize) ->
    case ?IS_OF_BITSIZE(Z, Dimensions * ComponentBitsize) of
        true ->
            ok;

        false ->
            error({badarg, Z})
    end.

component_bitsize(MinComponent, MaxComponent) ->
    Range = MaxComponent - MinComponent,
    unsigned_integer_bitsize(Range).

-spec encode_list_(Vector, Dimensions, MinComponent,
              MaxComponent, ComponentBitsize) -> Z
        when Vector :: list_vector(),
             Dimensions :: non_neg_integer(),
             MinComponent :: integer(),
             MaxComponent :: integer(),
             ComponentBitsize :: non_neg_integer(),
             Z :: non_neg_integer().
%% @doc Encodes `Vector', a tuple or list of `Dimensions`
%% Returns a pre-validated config which `:encode/2' and `:decode/2'` can use.
encode_list_(Vector, Dimensions, MinComponent, MaxComponent, ComponentBitsize) ->
    % Make values unsigned
    NormalizedVector =
        [validate_component(V, MinComponent, MaxComponent) - MinComponent
         || V <- Vector],
    interleave(NormalizedVector, Dimensions, ComponentBitsize).

-spec interleave(Values, Dimensions, Bitsize) -> Interleaved
        when Values :: list_vector(),
             Dimensions :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             Interleaved :: non_neg_integer().
interleave(Values, Dimensions, Bitsize) ->
    interleave_recur(Values, Dimensions, Bitsize, Bitsize, 0).

-spec interleave_recur(Values, Dimensions, TotalBitsize, BitIndex, Acc) -> Interleaved
        when Values :: list_vector(),
             Dimensions :: non_neg_integer(),
             TotalBitsize :: non_neg_integer(),
             BitIndex :: non_neg_integer(),
             Acc :: non_neg_integer(),
             Interleaved :: non_neg_integer().
interleave_recur(_Values, _Dimensions, _TotalBitsize, 0 = _BitIndex, Acc) ->
    Acc;
interleave_recur(Values, Dimensions, TotalBitsize, BitIndex, Acc) ->
    {Conjugated, NewValues} = conjugate_values(Values),
    ShiftAmount = Dimensions * (TotalBitsize - BitIndex),
    ShiftedConjugated = Conjugated bsl ShiftAmount,
    NewAcc = Acc bor ShiftedConjugated,
    interleave_recur(NewValues, Dimensions, TotalBitsize, BitIndex - 1, NewAcc).

-spec conjugate_values(Values) -> {Conjugated, NewValues}
        when Values :: [non_neg_integer()],
             Conjugated :: non_neg_integer(),
             NewValues :: [non_neg_integer()].
conjugate_values(Values) ->
    conjugate_values_recur(Values, 0, 0, []).

-spec conjugate_values_recur(Values, DimensionIndex, AccConjugated, AccNewValues)
    -> {Conjugated, NewValues}
        when Values :: list_vector(),
             DimensionIndex :: non_neg_integer(),
             AccConjugated :: non_neg_integer(),
             AccNewValues :: list_vector(),
             Conjugated :: non_neg_integer(),
             NewValues :: list_vector().
conjugate_values_recur([], _DimensionIndex, AccConjugated, AccNewValues) ->
    {AccConjugated, lists:reverse(AccNewValues)};
conjugate_values_recur([H | T], DimensionIndex, AccConjugated, AccNewValues) ->
    Bit = (H band 1) bsl DimensionIndex,
    NewH = H bsr 1,
    conjugate_values_recur(T, DimensionIndex + 1, Bit bor AccConjugated, [NewH | AccNewValues]).

-spec decode_list_(Z, Dimensions, MinComponent, MaxComponent,
                   Bitsize) -> Vector
        when Z :: non_neg_integer(),
             Dimensions :: non_neg_integer(),
             MinComponent :: integer(),
             MaxComponent :: integer(),
             Bitsize :: non_neg_integer(),
             Vector :: list_vector().
decode_list_(Z, Dimensions, MinComponent, MaxComponent, Bitsize) ->
    {NewZ, NormalizedVector} = revert_interleave(Z, Dimensions, Bitsize),

    (NewZ =:= 0 orelse error({badarg, Z})), % some higher bits weren't processed
    lists:map(
      fun (NormalizedValue) ->
              Value = MinComponent + NormalizedValue,
              (Value =< MaxComponent orelse error({badarg, Z})), % out of range
              Value
      end,
      NormalizedVector).

-spec revert_interleave(Interleaved, Dimensions, Bitsize) -> {NewInterLeaved, Values}
        when Interleaved :: non_neg_integer(),
             Dimensions :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             NewInterLeaved :: non_neg_integer(),
             Values :: [non_neg_integer()].
revert_interleave(Interleaved, Dimensions, Bitsize) ->
    Acc0 = repeat(0, Dimensions),
    revert_interleave_recur(Interleaved, Dimensions, Bitsize, 0, Acc0).

-spec revert_interleave_recur(Interleaved, Dimensions, Bitsize, BitIndex,
                              Acc) -> {NewInterLeaved, FinalAcc}
        when Interleaved :: non_neg_integer(),
             Dimensions :: non_neg_integer(),
             Bitsize :: non_neg_integer(),
             BitIndex :: non_neg_integer(),
             Acc :: [non_neg_integer()],
             NewInterLeaved :: non_neg_integer(),
             FinalAcc :: [non_neg_integer()].
revert_interleave_recur(Interleaved, _Dimensions, Bitsize, BitIndex, Acc)
  when BitIndex >= Bitsize ->
    {Interleaved, Acc};
revert_interleave_recur(Interleaved, Dimensions, Bitsize, BitIndex, Acc) ->
    {NewInterLeaved, NewAcc} = unchain_values(Interleaved, Bitsize, BitIndex, Acc),
    revert_interleave_recur(NewInterLeaved, Dimensions, Bitsize,
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

-spec validate_component(Value, Min, Max) -> Value
        when Value :: integer(),
             Min :: integer(),
             Max :: integer().
validate_component(Value, Min, Max) when is_integer(Value) ->
    case Value of
        _ when Value > Max ->
            error({vector_component_higher_than_max, [{value, Value}, {max, Max}]});
        _ when Value < Min ->
            error({vector_component_lower_than_min, [{value, Value}, {min, Min}]});
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
