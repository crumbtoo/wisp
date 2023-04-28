SOURCE := $(shell ls src/*.hs | grep -v main.hs)
all: build wisp

build:
	mkdir build || true

build/wisp.hs: src/wisp.y
	happy -a -g -c -o $@ $<

wisp: build/wisp.hs $(SOURCE)
	ghc -fglasgow-exts -outputdir=build -o $@ $< $(SOURCE)

.PHONY:
clean:
	rm -rf build
