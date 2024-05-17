SHELL := /bin/sh

.PHONY: build hoogle format haddock format_nix format_haskell format_check test

SOURCE_FILES := $(shell git ls-tree -r HEAD --full-tree --name-only)
SOURCE_FILES := $(wildcard $(SOURCE_FILES))
HASKELL_SOURCES := $(filter %.hs,$(SOURCE_FILES))
CABAL_SOURCES := $(filter %.cabal,$(SOURCE_FILES))
NIX_SOURCES := $(filter %.nix,$(SOURCE_FILES))
FORMAT_EXTENSIONS := -o -XQuasiQuotes -o -XTemplateHaskell	\
	-o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms	\
	-o -XOverloadedRecordDot
HLINT_EXTS := -XQuasiQuotes

THREADS ?= 8
TEST_CASE_TIMEOUT ?= 100

hoogle: haddock
	pkill hoogle || true
	hoogle generate --local=haddock --database=hoo/local.hoo
	hoogle server --local -p 8082 >> /dev/null &
	hoogle server --local --database=hoo/local.hoo -p 8083 >> /dev/null &

format: format_haskell format_nix

format_nix:
	nixpkgs-fmt $(NIX_SOURCES)

format_haskell:
	fourmolu $(FORMAT_EXTENSIONS) -m inplace $(HASKELL_SOURCES)
	cabal-fmt -i $(CABAL_SOURCES)

format_check:
	fourmolu $(FORMAT_EXTENSIONS) -m check $(HASKELL_SOURCES)
	nixpkgs-fmt --check $(NIX_SOURCES) 
	cabal-fmt --check $(CABAL_SOURCES)

haddock:
	cabal haddock elabs-backend --haddock-html --haddock-hoogle --builddir=haddock

test:
	cabal test elabs-backend -j$(THREADS)

integration-test:
	pkill cardano-node	|| true
	sh ./scripts/cleanup-node.sh
	sh ./scripts/run-privnet-test.sh

build:
	cabal build elabs-backend -j$(THREADS)

swagger:
	cabal run elabs-backend:app -j$(THREADS) -- swagger

run:
	cabal run elabs-backend:app -j$(THREADS) -- run

show-internal-address:
	cabal run elabs-backend:cli -- wallet internal-address

show-internal-collateral-address:
	cabal run elabs-backend:cli -- wallet internal-address --collateral

create-oracle:
	cabal run elabs-backend:app -- createOracle --rate 500000
