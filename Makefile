export ERL_FLAGS = -enable-feature maybe_expr # needed for katana-code under OTP 25

.PHONY: all build clean check
.PHONY: xref hank-dead-code-cleaner elvis-linter dialyzer
.PHONY: test cover
.PHONY: shell console doc-dry publish

.NOTPARALLEL: check cover test

all: build

build:
	@rebar3 compile

clean:
	@rebar3 clean

check: xref hank-dead-code-cleaner elvis-linter dialyzer

xref:
	@rebar3 xref

hank-dead-code-cleaner:
	@if rebar3 plugins list | grep '^rebar3_hank\>' >/dev/null; then \
		rebar3 hank; \
	else \
		echo >&2 "WARN: skipping rebar3_hank check"; \
	fi

elvis-linter:
	@if rebar3 plugins list | grep '^rebar3_lint\>' >/dev/null; then \
		rebar3 lint; \
	else \
		echo >&2 "WARN: skipping rebar3_lint check"; \
	fi

dialyzer:
	@rebar3 dialyzer

test: test/erlzord_tests.erl
	@rebar3 do eunit, ct, cover

cover: test

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

clean-test-data:
	rm support/test_cases/test_data/*.data

shell: export ERL_FLAGS = +pc unicode
shell:
	@rebar3 as shell shell

console: shell

doc-dry:
	@rebar3 hex build

publish:
	@rebar3 hex publish
