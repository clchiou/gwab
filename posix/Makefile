GWAB := $(OUT)/gwab

all: $(GWAB)

$(GWAB) : Main.hs
	ghc -o $@ -odir $(OUT) -hidir $(OUT) -i$(LIB) --make Main.hs
