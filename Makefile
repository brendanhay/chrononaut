#
# Vars
#

SHELL := /usr/bin/env bash
DEV   := cabal-dev

#
# Standard
#

.PHONY: clean install build dist

build: .devel
	$(DEV) build

clean: reconfigure
	$(DEV) clean

install:
	$(DEV) install

dist: Setup.hs
	runhaskell Setup.hs sdist

all: install lint unit

#
# Testing
#

.PHONY: lint unit integration benchmark

lint:
	hlint src

#
# Development
#

.PHONY: ghci

ghci:
	$(DEV) ghci

#
# Patterns
#

reconfigure:
	-rm -f .devel

.devel:
	$(MAKE) reconfigure
	$(DEV) configure && touch .devel
