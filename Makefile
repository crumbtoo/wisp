SOURCE := $(wildcard src/*.hs)

all: build wisp

build:
	mkdir build || true

build/wisp.hs: src/wisp.y
	happy -a -g -c -o $@ $<

wisp: build/wisp.hs $(SOURCE)
	ghc -outputdir=build -o $@ $< $(SOURCE)

.PHONY:
clean:
	rm -rf build

