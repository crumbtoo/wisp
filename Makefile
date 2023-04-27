all: wisp

wisp.hs: wisp.y
	happy -a -g -c -o $@ $<

wisp: wisp.hs main.hs
	ghc -fglasgow-exts $<

.PHONY:
clean:
	rm -rf *.hi *.o wisp wisp.hs
