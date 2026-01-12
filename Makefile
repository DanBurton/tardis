# This Makefile is only for testing with mhs.
# For general development, use `stack test` instead.

# Don't know how to install mcabal?
# See .github/workflows/mhs.yml

.PHONY: mhs-deps

mhs-deps:
	mcabal install ghc-compat
	mcabal install transformers
	mcabal install mtl
	mcabal install --git=https://github.com/ekmett/transformers-compat.git transformers-compat
	mcabal install mmorph

mhs-test:
	mcabal test
