#!/usr/bin/env python
import itertools
import sys
import random
import zorder

PRNG = random.SystemRandom()
COUNT = 1000

coord_dimension = long(sys.argv[1])
coord_min_value = long(sys.argv[2])
coord_max_value = long(sys.argv[3])
assert coord_max_value >= coord_min_value
coord_range = coord_max_value - coord_min_value
coord_bitsize = coord_range.bit_length()

encoder = zorder.ZEncoder(ndim=coord_dimension, bits=(coord_bitsize * coord_dimension))

def cull(v):
    return max(coord_min_value, min(coord_max_value, v))

def encode(coordinates):
    coordinates = map(lambda v: cull(v) - coord_min_value, coordinates)
    return encoder.encode(coordinates)

def repeat(fun, n):
    return [fun() for _ in range(n)]

def run():
    max_coord_count = (coord_range + 1) ** coord_dimension
    count = min(COUNT, max_coord_count)

    print >>sys.stderr, (
            'generating %d %dd values for coordinates between [%d, %d]' %
            (count, coord_dimension, coord_min_value, coord_max_value))

    if count >= max_coord_count:
        # generate sequentially
        coord_value_possibilities = range(coord_min_value, coord_max_value + 1)
        all_coordinates = itertools.product(
                coord_value_possibilities, repeat = coord_dimension)
    else:
        # brute force
        all_coordinates = set()
        while len(all_coordinates) < COUNT:
            coordinates = tuple(
                    repeat(
                        lambda: PRNG.randint(coord_min_value, coord_max_value),
                        coord_dimension))
            all_coordinates.add(coordinates)
        all_coordinates = sorted(list(all_coordinates))

    #print
    for coordinates in all_coordinates:
        coordinates_str = '{%s}' % ','.join(map(str, coordinates))
        print '{%s, %d}.' % (coordinates_str, encode(coordinates))

run()
