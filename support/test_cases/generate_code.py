#!/usr/bin/env python3
import parse
import os
import sys

def eunit_safe_number_str(v):
    if v < 0:
        return 'minus%s' % abs(v)
    else:
        return str(v)


test_data_filepaths = sys.argv[1:-1]
code_output_filepath = sys.argv[-1]
code_output_filename = os.path.basename(code_output_filepath)
module_name = parse.parse('{:w}.erl', code_output_filename)[0]
output = ''

output += '''-module({module_name}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> ok.

'''.format(
        module_name=module_name
        )

for path in test_data_filepaths:
    with open(path, 'r') as test_data_file:
        test_data_filename = os.path.basename(path)
        parse_result = parse.parse(
                '{:d}dim_from_{:d}_to_{:d}.data', test_data_filename)
        (coord_dim, coord_from, coord_to) = parse_result
        function_name = '\'%dd_from_%s_to_%s_test_\'' % (
                coord_dim,
                eunit_safe_number_str(coord_from),
                eunit_safe_number_str(coord_to))
        output += '''
-spec {function_name}() -> fun(() -> ok).
{function_name}() ->
    fun () ->
        Config = erlzord:config(#{{
            dimension => {coord_dim},
            min_coordinate_value => {coord_from},
            max_coordinate_value => {coord_to}
        }}),

        ?assertEqual(
            Config,
            erlzord:config(#{{
                dimensions => {coord_dim},
                min_coordinate_value => {coord_from},
                max_coordinate_value => {coord_to}
            }})
        ),

        ?assertEqual(
            Config,
            erlzord:config({coord_dim}, {coord_from}, {coord_to})
        ),

        {{ok, Terms}} = file:consult("{erl_test_data_filepath}"),
        lists:foreach(
            fun ({{Coordinates, ExpectedValue}}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({{Coordinates, ExpectedValue}}, {{Coordinates, Value}}),

                ListValue = erlzord:encode(tuple_to_list(Coordinates), Config),
                ?assertEqual({{Coordinates, ExpectedValue}}, {{Coordinates, ListValue}}),


                DirectValue = erlzord:encode(Coordinates, {coord_from}, {coord_to}),
                ?assertEqual({{Coordinates, ExpectedValue}}, {{Coordinates, DirectValue}}),

                DirectValueList = erlzord:encode(tuple_to_list(Coordinates), {coord_from}, {coord_to}),
                ?assertEqual({{Coordinates, ExpectedValue}}, {{Coordinates, DirectValueList}}),


                LegacyDirectValue = erlzord:encode(Coordinates, {coord_dim}, {coord_from}, {coord_to}),
                ?assertEqual({{Coordinates, ExpectedValue}}, {{Coordinates, LegacyDirectValue}}),

                LegacyDirectListValue = erlzord:encode(tuple_to_list(Coordinates), {coord_dim}, {coord_from}, {coord_to}),
                ?assertEqual({{Coordinates, ExpectedValue}}, {{Coordinates, LegacyDirectListValue}}),


                DecodedTupleCoordinates = erlzord:decode_tuple(Value, Config),
                ?assertEqual({{Value, DecodedTupleCoordinates}}, {{Value, Coordinates}}),

                DecodedListCoordinates = erlzord:decode_list(Value, Config),
                ?assertEqual({{Value, list_to_tuple(DecodedListCoordinates)}}, {{Value, Coordinates}}),

                DecodedLegacyTupleCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({{Value, DecodedLegacyTupleCoordinates}}, {{Value, Coordinates}}),


                DirectDecodedTupleCoordinates = erlzord:decode_tuple(Value, {coord_dim}, {coord_from}, {coord_to}),
                ?assertEqual({{Value, DirectDecodedTupleCoordinates}}, {{Value, Coordinates}}),

                DirectDecodedListCoordinates = erlzord:decode_list(Value, {coord_dim}, {coord_from}, {coord_to}),
                ?assertEqual({{Value, list_to_tuple(DirectDecodedListCoordinates)}}, {{Value, Coordinates}}),

                DirectDecodedLegacyTupleCoordinates = erlzord:decode(Value, {coord_dim}, {coord_from}, {coord_to}),
                ?assertEqual({{Value, DirectDecodedLegacyTupleCoordinates}}, {{Value, Coordinates}})
            end,
            Terms)
    end.
'''.format(
        function_name=function_name,
        coord_dim=coord_dim,
        coord_from=coord_from,
        coord_to=coord_to,
        erl_test_data_filepath=path)

output += '''
-endif.'''

with open(code_output_filepath, 'w') as output_file:
    output_file.write(output)
