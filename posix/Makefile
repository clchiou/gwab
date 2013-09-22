GWAB := $(OUT)/gwab
ARGS := -outputdir $(OUT) -i$(LIB)

all: $(GWAB)

$(GWAB) : Main.hs
	ghc $(ARGS) -o $@ --make $<
