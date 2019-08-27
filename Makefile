all: build

build: bin/toylang
	stack build

run: bin/toylang 
	stack build
	stack exec toylang

clean:
	stack clean

bin/toylang: bin Main.hs
	stack install --local-bin-path bin

code:
	stack build hoogle intero stylish-haskell hlint hoogle; \
	$(SHELL) -c -i "code ."

.PHONY: all run clean
