#
# Vars
#

SHELL := /usr/bin/env bash
DEV   := cabal-dev

#
# Standard
#

.PHONY: build clean install

all: build lint

build: .devel
	$(DEV) build

clean: reconfigure
	$(DEV) clean

install:
	$(DEV) install

#
# Testing
#

.PHONY: lint

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
