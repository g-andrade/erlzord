SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

## General Rules

all: compile
.PHONY: all
.NOTPARALLEL: all

compile:
	@rebar3 compile
.PHONY: compile

clean:
	@rebar3 clean -a
.PHONY: clean

check: check-fast check-slow
.NOTPARALLEL: check
.PHONY: check

check-fast: check-formatted xref hank-dead-code-cleaner elvis-linter
.NOTPARALLEL: check-fast
.PHONY: check-fast

check-slow: dialyzer
.NOTPARALLEL: check-slow
.PHONY: check-slow

test: eunit
.NOTPARALLEL: test
.PHONY: test

format:
	@rebar3 fmt
.NOTPARALLEL: format
.PHONY: format

## Tests

# The eunit suite is machine-generated from the test-case oracles under
# support/test_cases/ (see test-data below). The generated file is committed,
# so CI uses it as-is; regeneration needs python3 and is a local-only step.
eunit: test/erlzord_tests.erl
	@rebar3 do eunit, cover
.PHONY: eunit

test/erlzord_tests.erl:
	./support/test_cases/generate_code.py support/test_cases/test_data/*.data test/erlzord_tests.erl

test-data: clean-test-data
	./support/test_cases/generate_data.py  1          0         7 >support/test_cases/test_data/1dim_from_0_to_7.data
	./support/test_cases/generate_data.py  2          0         7 >support/test_cases/test_data/2dim_from_0_to_7.data
	./support/test_cases/generate_data.py  3          0         7 >support/test_cases/test_data/3dim_from_0_to_7.data
	./support/test_cases/generate_data.py  4          0         7 >support/test_cases/test_data/4dim_from_0_to_7.data
	./support/test_cases/generate_data.py  5          0         7 >support/test_cases/test_data/5dim_from_0_to_7.data
	./support/test_cases/generate_data.py 10          0         7 >support/test_cases/test_data/10dim_from_0_to_7.data
	./support/test_cases/generate_data.py 30          0         7 >support/test_cases/test_data/30dim_from_0_to_7.data
	./support/test_cases/generate_data.py  1          0       100 >support/test_cases/test_data/1dim_from_0_to_100.data
	./support/test_cases/generate_data.py  2          0       100 >support/test_cases/test_data/2dim_from_0_to_100.data
	./support/test_cases/generate_data.py  3          0       100 >support/test_cases/test_data/3dim_from_0_to_100.data
	./support/test_cases/generate_data.py  4          0       100 >support/test_cases/test_data/4dim_from_0_to_100.data
	./support/test_cases/generate_data.py  5          0       100 >support/test_cases/test_data/5dim_from_0_to_100.data
	./support/test_cases/generate_data.py 10          0       100 >support/test_cases/test_data/10dim_from_0_to_100.data
	./support/test_cases/generate_data.py 30          0       100 >support/test_cases/test_data/30dim_from_0_to_100.data
	./support/test_cases/generate_data.py  1          0 123456789 >support/test_cases/test_data/1dim_from_0_to_123456789.data
	./support/test_cases/generate_data.py  2          0 123456789 >support/test_cases/test_data/2dim_from_0_to_123456789.data
	./support/test_cases/generate_data.py  3          0 123456789 >support/test_cases/test_data/3dim_from_0_to_123456789.data
	./support/test_cases/generate_data.py  4          0 123456789 >support/test_cases/test_data/4dim_from_0_to_123456789.data
	./support/test_cases/generate_data.py  5          0 123456789 >support/test_cases/test_data/5dim_from_0_to_123456789.data
	./support/test_cases/generate_data.py 10          0 123456789 >support/test_cases/test_data/10dim_from_0_to_123456789.data
	./support/test_cases/generate_data.py 30          0 123456789 >support/test_cases/test_data/30dim_from_0_to_123456789.data
	./support/test_cases/generate_data.py  1 -123456789     54321 >support/test_cases/test_data/1dim_from_-123456789_to_54321.data
	./support/test_cases/generate_data.py  2 -123456789     54321 >support/test_cases/test_data/2dim_from_-123456789_to_54321.data
	./support/test_cases/generate_data.py  3 -123456789     54321 >support/test_cases/test_data/3dim_from_-123456789_to_54321.data
	./support/test_cases/generate_data.py  4 -123456789     54321 >support/test_cases/test_data/4dim_from_-123456789_to_54321.data
	./support/test_cases/generate_data.py  5 -123456789     54321 >support/test_cases/test_data/5dim_from_-123456789_to_54321.data
	./support/test_cases/generate_data.py 10 -123456789     54321 >support/test_cases/test_data/10dim_from_-123456789_to_54321.data
	./support/test_cases/generate_data.py 30 -123456789     54321 >support/test_cases/test_data/30dim_from_-123456789_to_54321.data
.PHONY: test-data

clean-test-data:
	rm support/test_cases/test_data/*.data
.PHONY: clean-test-data

## Checks

check-formatted:
	@if rebar3 plugins list | grep '^erlfmt\>' >/dev/null; then \
		rebar3 fmt --check; \
	else \
		echo >&2 "WARN: skipping rebar3 erlfmt check"; \
	fi
.NOTPARALLEL: check-formatted
.PHONY: check-formatted

xref:
	@rebar3 xref
.PHONY: xref

hank-dead-code-cleaner:
	@if rebar3 plugins list | grep '^rebar3_hank\>' >/dev/null; then \
		rebar3 hank; \
	else \
		echo >&2 "WARN: skipping rebar3_hank check"; \
	fi
.PHONY: hank-dead-code-cleaner

elvis-linter:
	@if rebar3 plugins list | grep '^rebar3_lint\>' >/dev/null; then \
		rebar3 lint; \
	else \
		echo >&2 "WARN: skipping rebar3_lint check"; \
	fi
.PHONY: elvis-linter

dialyzer:
	@rebar3 dialyzer
.PHONY: dialyzer

## Shell, docs and publication

publish: doc
publish:
	@rebar3 hex publish --doc-dir=doc
.NOTPARALLEL: publish

shell: export ERL_FLAGS = +pc unicode
shell:
	@rebar3 as shell shell

doc: SOURCE_REF := $(shell git describe --tags --exact-match 2>/dev/null || git rev-parse --short HEAD)
doc: tmp/ex_doc
doc:
	rebar3 edoc; \
		./tmp/ex_doc "erlzord" "${SOURCE_REF}" \
		_build/docs/lib/erlzord/ebin \
		-c ex_doc.config \
		--source-ref "${SOURCE_REF}";
.PHONY: doc

tmp/ex_doc: EX_DOC_VER=0.40.2
tmp/ex_doc: OTP_VER := $(shell erl -noshell -eval 'io:fwrite("~s", [erlang:system_info(otp_release)]), init:stop().')
tmp/ex_doc: | tmp
tmp/ex_doc:
	curl -fL -o tmp/ex_doc \
		"https://github.com/elixir-lang/ex_doc/releases/download/v${EX_DOC_VER}/ex_doc_otp_${OTP_VER}"; \
		chmod a+x tmp/ex_doc

tmp:
	mkdir tmp
