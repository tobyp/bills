bills: bills.hs
	ghc -o $@ $<

check:
	./test.sh tests
