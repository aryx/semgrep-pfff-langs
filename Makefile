###############################################################################
# semgrep-pfff-langs Makefile
###############################################################################

# Set environment variables for tree-sitter C headers and libraries.
# This file is created by scripts/setup-tree-sitter.sh.
-include tree-sitter-config.mk

.PHONY: default
default:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: test
test:
	dune runtest

.PHONY: install
install:
	dune install

.PHONY: setup
setup:
	git submodule update --init --recursive
	./scripts/setup-tree-sitter.sh
	opam install --confirm-level=unsafe-yes --deps-only .
