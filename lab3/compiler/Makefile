all: l2c

l2c:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build
	rm -rf bin/
	cp -r shellbins/ bin/
	chmod +x bin/*
clean:
	rm -rf bin/
	rm -rf dist/
