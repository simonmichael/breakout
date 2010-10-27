BUILDFLAGS= #-prof -auto-all # -O2
TIME=`date +"%Y%m%d%H%M"`
TOPROFILE=./breakout

build: Tags breakout

# build with sdl wrapper for mac osx (see mainc.c or hssdl/Examples/MacOSX)
PROGNAME=breakout
$(PROGNAME): mainc.o MainWrapper.hs Main.hs
	ghc -no-hs-main --make mainc.o MainWrapper.hs -o $@
mainc.o: mainc.c MainWrapper_stub.h
	ghc -no-hs-main `sdl-config --cflags` -Wall $*.c -c
MainWrapper_stub.h: MainWrapper.hs
	ghc -no-hs-main --make $< -c
clean:
	rm -f *.hi *.o *_stub.c *_stub.h $(PROGNAME)
.PHONY: clean
#




profile prof:
	$(BUILD) -prof -auto-all
	$(TOPROFILE) +RTS -p
	mv breakout.prof $(TIME).prof
	cat $(TIME).prof

xprofile xprof:
	$(BUILD) -prof -auto-all
	$(TOPROFILE) +RTS -px
	mv breakout.prof $(TIME).prof
	ghcprof $(TIME).prof

loc:
	@darcs trackdown 'find . -name "*hs" |xargs wc -l |echo OUTPUT `tail -1`; false' |ruby -nae'puts $$F[1] if /^OUTPUT/'

Tags:
	hasktags -e *.hs

Clean: clean
	rm -f breakout overview TAGS tags
