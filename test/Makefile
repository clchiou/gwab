TEST := $(OUT)/test
ARGS := -fhpc -outputdir $(OUT) -i$(LIB) -i$(shell pwd)/../posix

all:
	ghc $(ARGS) -o $(TEST) --make Main.hs

.PHONY: all
