BUILDFLAGS= # -O

build: Tags breakout

# build with sdl wrapper for mac osx (see mainc.c or hssdl/Examples/MacOSX)
PROGNAME=breakout
$(PROGNAME)p: mainc.o MainWrapper.hs Main.hs
	ghc -no-hs-main --make mainc.o MainWrapper.hs -o $@ $(BUILDFLAGS) -prof -auto-all
$(PROGNAME): mainc.o MainWrapper.hs Main.hs
	ghc -no-hs-main --make mainc.o MainWrapper.hs -o $@ $(BUILDFLAGS)
mainc.o: mainc.c MainWrapper_stub.h
	ghc -no-hs-main `sdl-config --cflags` -Wall $*.c -c
MainWrapper_stub.h: MainWrapper.hs
	ghc -no-hs-main --make $< -c
clean:
	rm -f *.hi *.o *_stub.c *_stub.h $(PROGNAME)
.PHONY: clean
#




#TIME:=`date +"%Y%m%d%H%M"`

profile: $(PROGNAME)p
	./$(PROGNAME)p +RTS -p
	cat $(PROGNAME)p.prof
#	mv $(PROGNAME)p.prof $(TIME).prof
#	cat $(TIME).prof

xprofile xprof:
	$(BUILD) -prof -auto-all
	./$(PROGNAME)p +RTS -px
	ghcprof $(PROGNAME)p.prof
#	mv $(PROGNAME)p.prof $(TIME).prof
#	ghcprof $(TIME).prof

loc:
	@darcs trackdown 'find . -name "*hs" |xargs wc -l |echo OUTPUT `tail -1`; false' |ruby -nae'puts $$F[1] if /^OUTPUT/'

Tags:
	hasktags -e *.hs

Clean: clean
	rm -f breakout overview TAGS tags
