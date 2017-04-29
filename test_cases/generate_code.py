#!/usr/bin/env python2
import parse
import os
import sys

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
        function_name = '\'%s_test_\'' % test_data_filename
        parse_result = parse.parse(
                '{:d}dim_from_{:d}_to_{:d}.data', test_data_filename)
        (coord_dim, coord_from, coord_to) = parse_result
        output += '''
-spec {function_name}() -> fun(() -> ok).
{function_name}() ->
    fun () ->
        Config = erlzord:config({coord_dim}, {coord_from}, {coord_to}),
        {{ok, Terms}} = file:consult("{erl_test_data_filepath}"),

        lists:foreach(
            fun ({{Coordinates, ExpectedValue}}) ->
                Value = erlzord:encode(Coordinates, Config),
                ?assertEqual({{Coordinates, ExpectedValue}}, {{Coordinates, Value}}),

                DirectValue = erlzord:encode(Coordinates, {coord_dim}, {coord_from}, {coord_to}),
                ?assertEqual({{Coordinates, ExpectedValue}}, {{Coordinates, DirectValue}}),

                DecodedCoordinates = erlzord:decode(Value, Config),
                ?assertEqual({{Value, DecodedCoordinates}}, {{Value, Coordinates}}),

                DirectDecodedCoordinates = erlzord:decode(Value, {coord_dim}, {coord_from}, {coord_to}),
                ?assertEqual({{Value, DirectDecodedCoordinates}}, {{Value, Coordinates}})
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
