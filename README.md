

# erlzord #

Copyright (c) 2017 Guilherme Andrade

__Authors:__ Guilherme Andrade ([`erlzord(at)gandrade(dot)net`](mailto:erlzord(at)gandrade(dot)net)).

`erlzord`: N-dimensional Z-order curves for Erlang


---------

`erlzord` is a straightforward Erlang implementation of the [Z-order curve](https://en.wikipedia.org/wiki/Z-order_curve) function.

* Any number of dimensions is supported
* No limit on the bitsize of output values

The code was successfully tested on generations 17, 18 and 19 of Erlang/OTP; the test cases and their data
were automatically generated based on an existing [Python implementation](https://github.com/LLNL/rubik/blob/master/rubik/zorder.py)
from the [rubik](https://github.com/LLNL/rubik) project.


### <a name="Examples">Examples</a> ###


```erlang

% 1d points
Config = erlzord:config(1, 0, 100), % Coordinate values between 0 and 100
erlzord:encode({42}, Config),       % 42
erlzord:decode(42, Config).         % {42}

% 2d points
Config = erlzord:config(2, 0, 100),
erlzord:encode({50,0}, Config), % 1284
erlzord:decode(1284,   Config). % {50,0}

% 3d points
Config = erlzord:config(3, 0, 100),
erlzord:encode({0,0,100}, Config), % 1179904
erlzord:decode(1179904,   Config). % {0,0,100}

% 10d points
Config = erlzord:config(10, 0, 100),
erlzord:encode(
    {100, 100, 100, 100, 100, 100, 100,
     100, 100, 100}, Config), % 1180590494818577154048
erlzord:decode(
    1180590494818577154048, Config). % {100,100,..,100}.

```


### <a name="TODO">TODO</a> ###

* Range searches


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/erlzord/blob/master/doc/erlzord.md" class="module">erlzord</a></td></tr></table>

