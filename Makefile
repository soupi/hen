.PHONY: setup

setup:
	stack setup

.PHONY: build

build:
	stack build

.PHONY: dev

dev:
	stack build --fast --file-watch

.PHONY: run

run:
	stack exec hen

.PHONY: clean

clean:
	stack clean
