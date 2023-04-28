all: build wisp

build:
	mkdir build || true

build/wisp.hs: src/wisp.y
	happy -a -g -c -o $@ $<

wisp: build/wisp.hs src/main.hs
	ghc -fglasgow-exts -o $@ $< src/StackMonad.hs

.PHONY:
clean:
	rm -rf build
