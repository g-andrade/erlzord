# Z-order curve / Morton code for Erlang

[![Hex downloads](https://img.shields.io/hexpm/dt/erlzord.svg)](https://hex.pm/packages/erlzord)
[![License](https://img.shields.io/hexpm/l/erlzord.svg)](https://github.com/g-andrade/erlzord/blob/main/LICENSE)
[![Erlang Versions](https://img.shields.io/badge/Erlang%2FOTP-24%20to%2029-blue)](https://www.erlang.org)
[![CI status](https://github.com/g-andrade/erlzord/actions/workflows/ci.yml/badge.svg)](https://github.com/g-andrade/erlzord/actions/workflows/ci.yml)
[![Latest version](https://img.shields.io/hexpm/v/erlzord.svg?style=flat)](https://hex.pm/packages/erlzord)
[![API reference](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/erlzord/)
[![Last commit](https://img.shields.io/github/last-commit/g-andrade/erlzord.svg)](https://github.com/g-andrade/erlzord/commits/main)

`erlzord` is an implementation of the [Z-order curve](https://en.wikipedia.org/wiki/Z-order_curve),
or Morton code, for Erlang and Elixir.

Any number of dimensions is supported and there is no limit on the bitsize of output values.

The test cases were generated based on an existing [Python implementation](https://github.com/LLNL/rubik/blob/master/rubik/zorder.py) from the [rubik](https://github.com/LLNL/rubik) project.

### How to use

#### Erlang

##### 1\. Import as a dependency

rebar.config

```erlang
{deps, [
    % [...]
    {erlzord, "~> 2.0"}
]}.
```

your\_application.app.src

```erlang
{applications, [
    kernel,
    stdlib,
    % [...]
    erlzord
]}
```

##### 2\. Encode and decode your vectors

2d points

```erlang
Config = erlzord:config(#{
    dimensions => 2,
    min_component => -1000,
    max_component => +1000
}),

1595729 = erlzord:encode({245, -456}, Config),
{245,-456} = erlzord:decode(1595729, Config).
```

3d points

```erlang
Config = erlzord:config(#{
    dimensions => 3,
    min_component => 0,
    max_component => 100
}),

123788 = erlzord:encode({42, 52, 21}, Config),
{42,52,21} = erlzord:decode_tuple(123788, Config).
```

10d points

```erlang
Config = erlzord:config(#{
    dimensions => 10,
    min_component => 0,
    max_component => 700
}),

740763791023146735114306653082 =
    erlzord:encode([400,543,632,445,565,0,674,491,403,555], Config),

[400,543,632,445,565,0,674,491,403,555] =
    erlzord:decode_list(740763791023146735114306653082, Config).
```

#### Elixir

##### 1\. Import as a dependency

mix.exs

``` elixir
  defp deps do
    [
      # [...]
      {:erlzord, "~> 2.0"}
    ]
  end
```

##### 2\. Encode and decode your vectors

2d points

```elixir
config = :erlzord.config(%{
    dimensions: 2,
    min_component: -1000,
    max_component: +1000
})

1595729 = :erlzord.encode({245,-456}, config)
{245,-456} = :erlzord.decode_tuple(1595729, config)
```

3d points

```elixir
config = :erlzord.config(%{
    dimensions: 3,
    min_component: 0,
    max_component: 100
})

123788 = :erlzord.encode({42,52,21}, config)
{42,52,21} = :erlzord.decode_tuple(123788, config)
```

10d points

```elixir
config = :erlzord.config(%{
    dimensions: 10,
    min_component: 0,
    max_component: 700
})

740763791023146735114306653082 =
    :erlzord.encode([400,543,632,445,565,0,674,491,403,555], config)

[400,543,632,445,565,0,674,491,403,555] =
    :erlzord.decode_list(740763791023146735114306653082, config)
```

### API Reference

The API reference can be found on [HexDocs](https://hexdocs.pm/erlzord/).

### License

MIT License

Copyright (c) 2017-2026 Guilherme Andrade

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
