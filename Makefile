# This Makefile is only for testing with microhs.
# For general development, use `stack test` instead.

# Don't know how to install mcabal?
# See .github/workflows/microhs.yml

.PHONY: microhs-deps

microhs-deps:
	mcabal install ghc-compat
	mcabal install transformers
	mcabal install mtl
	mcabal install --git=https://github.com/ekmett/transformers-compat.git transformers-compat
	mcabal install mmorph

microhs-test:
	mcabal test
