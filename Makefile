.PHONY: build
build:
	dune build

run: build
	time _build/default/ogrep.exe ../linux
