all: l4c

l4c:
	runhaskell Setup.hs configure --enable-optimization=2
	runhaskell Setup.hs build 
	rm -rf bin/
	cp -r shellbins/ bin/
	chmod +x bin/*
clean:
	rm -rf bin/
	rm -rf dist/
