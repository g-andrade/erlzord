# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0.0] - 2026-06-07

### Added

- support for OTP 24 through 29
- map-based `config/1` plus `encode/2`, `decode_tuple/2`, `decode_list/2`,
  `encode/3`, `decode_tuple/4` and `decode_list/4`
- API documentation rendered with ex_doc (EEP-48 native docs)

### Changed

- ⚠️ out-of-bounds vector components now raise an exception instead of being
  silently culled (see MIGRATION.md)
- the API now speaks of "vectors" rather than "coordinates"
- CI from Travis to GitHub Actions
- development tooling: erlfmt, rebar3_hank, rebar3_lint (Elvis) and Dialyzer

### Deprecated

- `config/3`, `decode/2`, `encode/4` and `decode/4`

### Removed

- ⚠️ **culling of values** (see MIGRATION.md)
- support for OTP 19 through 23

## [1.1.0] - 2020-05-26

### Added

- OTP 22.0 to CI
- OTP 22.1 to CI
- OTP 22.2 to CI
- OTP 22.3 to CI
- OTP 23.0 to CI

### Removed

- OTP 17 from CI
- OTP 18 from CI

## [1.0.3] - 2019-01-19

### Added

- Travis CI

## [1.0.2] - 2017-04-30

### Added

- inference of app version from git

## [1.0.1] - 2017-04-30

### Fixed

- unwarranted import of `rebar3_hex` by lib dependents

## [1.0.0] - 2017-04-30

### Added

- initial implementation
