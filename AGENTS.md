# erlzord

Erlang/OTP library implementing the N-dimensional
[Z-order curve](https://en.wikipedia.org/wiki/Z-order_curve) (Morton code):
encoding a vector of integer components into a single interleaved value, and
decoding it back. Any number of dimensions is supported and there is no limit on
the bitsize of the encoded values. It is a pure, stateless library — no
processes, no application/supervision tree.

## Build, test, check

```bash
make compile         # compile
make test            # eunit (the only test kind here) + coverage
make check           # check-fast + check-slow
make check-fast      # format check (erlfmt) + xref + dead-code (hank) + lint (elvis)
make check-slow      # dialyzer
make format          # auto-format src with erlfmt
make eunit           # unit tests only
make doc             # EEP-48 docs via rebar3 edoc + the ex_doc escript -> doc/
make shell           # interactive REPL with the app started
```

All checks run sequentially (`.NOTPARALLEL`). CI runs `make check-fast`,
`make test`, and `make check-slow` on OTP 24–29 (Linux).

## Compiler flags

`debug_info`, `warn_export_vars`, `warn_missing_spec`, `warn_unused_import` and
`warnings_as_errors` are always on; every exported function must have a `-spec`.
The `test` and `shell` profiles relax `warn_missing_spec`/`warnings_as_errors`.

## Architecture

A single module, `src/erlzord.erl`:

| Concern | Functions |
|---|---|
| Config | `config/1` (map params) → opaque `t:config/0` |
| Encode | `encode/2` (config), `encode/3` (inline bounds) |
| Decode | `decode_tuple/2`, `decode_list/2`, `decode_tuple/4`, `decode_list/4` |
| Deprecated | `config/3`, `decode/2`, `encode/4`, `decode/4` |

Internally, `encode` interleaves component bits (`interleave*`, building up an
integer through `#z_builder{}`), and `decode` reverses the interleave
(`revert_interleave*`/`unchain_values*`). Public functions validate their inputs
and then delegate to a trailing-underscore "raw" worker (`config_`,
`encode_list_`, `decode_list_`) that assumes pre-validated arguments.

## Tests

`test/erlzord_tests.erl` is **machine-generated** and carries a "DO NOT EDIT"
banner — do not edit it by hand. It is a eunit suite generated from oracle data
files under `support/test_cases/`:

- `support/test_cases/generate_data.py` produces `*.data` files (vector →
  expected Z value) using a reference Python Z-order implementation (from LLNL's
  rubik project).
- `support/test_cases/generate_code.py` turns those `*.data` files into the
  eunit suite.

The generated suite is committed and used as-is in CI. Regenerate locally with
`make test/erlzord_tests.erl` (needs python3 and the `parse` package); regenerate
the oracle data with `make test-data`. Because it is generated, the suite is
excluded from erlfmt (`rebar.config` lists only `src/*` under `erlfmt.files`) and
relaxes `function_naming_convention`/`max_line_length` in `elvis.config`. There
are no Common Test suites, so `make test` is just eunit.

## Code conventions

- Code is formatted with `erlfmt`; run `make format` before committing. The
  reformat commit is recorded in `.git-blame-ignore-revs`.
- Documentation uses **EEP-48 native docs** (`-moduledoc`/`-doc`), guarded by
  `-ifdef(E48).` (OTP 27+, via the `E48` platform_define in `rebar.config`).
  Hide internal exported functions with `-doc false`, **not** `%% @private`
  (ex_doc ignores `@private`). Deprecated public functions keep a real `-doc`
  and rely on the `-deprecated` attribute for ex_doc's deprecation banner.
- The docs pipeline is `rebar3 edoc` (top-level `edoc_opts`, chunk doclet/layout)
  populating `_build/docs/lib/erlzord/ebin`, then the ex_doc escript renders it
  to `doc/`. There is no `docs` rebar3 profile.
- elvis exceptions are documented inline in `elvis.config`: the trailing-
  underscore worker naming in `src`, and the relaxations for the generated suite.

## OTP version notes

Supported range is OTP 24–29 (`minimum_otp_vsn` is declared low for dependents,
but only 24+ is supported). `rebar.config.script` drops dev plugins that no
longer build on old OTP: erlfmt + hank + elvis on OTP ≤ 25, and erlfmt alone on
OTP ≤ 26 (triple-quoted `-doc` strings break erlfmt/katana there). The Makefile
check targets degrade gracefully (skip with a WARN) when a plugin is absent.

## Releasing

`make publish` runs `rebar3 hex publish --doc-dir=doc`. Versioning follows
SemVer; history is in `CHANGELOG.md` (Keep a Changelog format). Breaking changes
are documented in `MIGRATION.md`.
