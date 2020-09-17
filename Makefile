ghci:
	stack repl --ghci-options='-fno-ghci-sandbox'

loc:
	cloc --vcs=git

etags:
	hasktags -e *.hs

