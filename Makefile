SOURCE := $(wildcard src/*.hs)

all: build protolush

build:
	mkdir build || true

build/Parser.hs: src/protolush.y
	happy -a -g -c -o $@ $<

protolush: build/Parser.hs $(SOURCE)
	ghc -package mtl -outputdir=build -o $@ $< $(SOURCE)

.PHONY:
clean:
	rm -rf build

