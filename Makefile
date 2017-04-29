REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

ifeq ($(wildcard rebar3),rebar3)
	REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

ifeq ($(REBAR3),)
	REBAR3 = $(CURDIR)/rebar3
endif

.PHONY: deps build test dialyzer xref test_code test_cases test_cases_clean

all: build

build: $(REBAR3)
	@$(REBAR3) compile

$(REBAR3):
	wget $(REBAR3_URL) || curl -Lo rebar3 $(REBAR3_URL)
	@chmod a+x rebar3

clean:
	@$(REBAR3) clean

dialyzer:
	@$(REBAR3) dialyzer

xref:
	@$(REBAR3) xref

test:
	@$(REBAR3) eunit

test_code:
	./test_cases/generate_code.py test_cases/test_data/*.data src/erlzord_test.erl

test_data:
	./test_cases/generate_data.py 1 0 7 >test_cases/test_data/1dim_from_0_to_7.data
	./test_cases/generate_data.py 2 0 7 >test_cases/test_data/2dim_from_0_to_7.data
	./test_cases/generate_data.py 3 0 7 >test_cases/test_data/3dim_from_0_to_7.data
	./test_cases/generate_data.py 4 0 7 >test_cases/test_data/4dim_from_0_to_7.data
	./test_cases/generate_data.py 5 0 7 >test_cases/test_data/5dim_from_0_to_7.data
	./test_cases/generate_data.py 10 0 7 >test_cases/test_data/10dim_from_0_to_7.data
	./test_cases/generate_data.py 30 0 7 >test_cases/test_data/30dim_from_0_to_7.data
	./test_cases/generate_data.py 1 0 100 >test_cases/test_data/1dim_from_0_to_100.data
	./test_cases/generate_data.py 2 0 100 >test_cases/test_data/2dim_from_0_to_100.data
	./test_cases/generate_data.py 3 0 100 >test_cases/test_data/3dim_from_0_to_100.data
	./test_cases/generate_data.py 4 0 100 >test_cases/test_data/4dim_from_0_to_100.data
	./test_cases/generate_data.py 5 0 100 >test_cases/test_data/5dim_from_0_to_100.data
	./test_cases/generate_data.py 10 0 100 >test_cases/test_data/10dim_from_0_to_100.data
	./test_cases/generate_data.py 30 0 100 >test_cases/test_data/30dim_from_0_to_100.data
	./test_cases/generate_data.py 1 0 12345679 >test_cases/test_data/1dim_from_0_to_12345679.data
	./test_cases/generate_data.py 2 0 12345679 >test_cases/test_data/2dim_from_0_to_12345679.data
	./test_cases/generate_data.py 3 0 12345679 >test_cases/test_data/3dim_from_0_to_12345679.data
	./test_cases/generate_data.py 4 0 12345679 >test_cases/test_data/4dim_from_0_to_12345679.data
	./test_cases/generate_data.py 5 0 12345679 >test_cases/test_data/5dim_from_0_to_12345679.data
	./test_cases/generate_data.py 10 0 12345679 >test_cases/test_data/10dim_from_0_to_12345679.data
	./test_cases/generate_data.py 30 0 12345679 >test_cases/test_data/30dim_from_0_to_12345679.data

test_cases_clean:
	rm test_cases/test_data/*.data