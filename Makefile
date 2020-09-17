ghci:
	stack repl --ghci-options='-fno-ghci-sandbox'

ghcid:
	ghcid -c 'stack repl'

# --restart needed for now to destroy the SDL window
rerun:
	ghcid -c 'stack repl' -r --restart=breakout.hs

loc:
	cloc --vcs=git

etags:
	hasktags -e *.hs

