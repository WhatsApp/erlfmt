.PHONY: compile
compile:
	rebar3 compile

.PHONY: test
test:
	rebar3 ct

.PHONY: release
release:
	rebar3 as release escriptize

.PHONY: clean
clean:
	rm -rf _build
	rebar3 clean

.PHONY: fmt
fmt:
	make clean
	make release
	_build/release/bin/erlfmt -w "{src,include,test}/*.{hrl,erl,app.src}" "rebar.config"

.PHONY: checkfmt
checkfmt:
	make release
	rm src/erlfmt_parse.erl
	_build/release/bin/erlfmt -c "{src,include,test}/*.{hrl,erl,app.src}" "rebar.config"

.PHONY: check
check:
	make compile
	make test
	rebar3 dialyzer
	make checkfmt
