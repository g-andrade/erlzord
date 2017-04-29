

# erlzord #

Copyright (c) 2017 Guilherme Andrade

__Version:__ 1.0.0

__Authors:__ Guilherme Andrade ([`erlzord(at)gandrade(dot)net`](mailto:erlzord(at)gandrade(dot)net)).

`erlzord`: N-dimensional Z-order curves for Erlang

---------

`erlzord` is a straightforward Erlang implementation of the [Z-order curve](https://en.wikipedia.org/wiki/Z-order_curve) function.

* Any number of dimensions is supported
* No limit on the bitsize of output values

The code was successfully tested on generations 17, 18 and 19 of Erlang/OTP; the test cases and their data
were automatically generated based on an existing [Python Implementation](https://github.com/LLNL/rubik/blob/master/rubik/zorder.py)
from the [rubik](https://github.com/LLNL/rubik) project.


### <a name="Examples">Examples</a> ###


```erlang

Config = erlzord:config(0, 100), % Coordinate values between 0 and 100

% 1d points
erlzord:calculate({0},   Config), % 0
erlzord:calculate({50},  Config), % 50
erlzord:calculate({100}, Config), % 100

% 2d points
erlzord:calculate({0, 0},     Config), % 0
erlzord:calculate({50, 0},    Config), % 1284
erlzord:calculate({0, 100},   Config), % 15408
erlzord:calculate({100, 100}, Config), % 15408

% 3d points
erlzord:calculate({0, 0, 0},       Config), % 0
erlzord:calculate({50, 0, 0},      Config), % 36872
erlzord:calculate({0, 0, 100},     Config), % 1179904
erlzord:calculate({100, 100, 100}, Config), % 2064832

% 10d points
erlzord:calculate(
    {100, 100, 100, 100, 100, 100, 100, 
     100, 100, 100}, Config). % 1180590494818577154048

```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="erlzord.md" class="module">erlzord</a></td></tr></table>

