include Makefile.base

.PHONY: debug-test
debug-test:
	PROP_UNIT_DEBUG=1 stack test --trace --profile

.PHONY: deep-test
deep-test:
	PROP_UNIT_LIMIT=1000 make test
