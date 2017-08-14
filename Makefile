build:
	jbuilder build @install

run: build
	jbuilder exec prime_sieves

test:
	jbuilder runtest

pin:
	opam pin add .

repin: build
	opam upgrade prime_sieves

build-all:
	jbuilder build --workspace jbuild-workspace.dev @install

test-all:
	jbuilder build --workspace jbuild-workspace.dev @runtest

.PHONY: build test pin repin build-all test-all