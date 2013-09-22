HASTEC ?= hastec

GWAB := $(OUT)/gwab.js
MAIN := $(shell pwd)/Main.hs
ARGS := -outputdir $(OUT) -i$(LIB)

all: $(GWAB)

# XXX: Changing directory to $(OUT) before calling hastec is a hack for placing
# intermediate object files there rather than here (because hastec does not
# provide a option for this).
$(GWAB) : Main.hs
	cd $(OUT) ; $(HASTEC) $(ARGS) --out=$@ --make $(MAIN)

.PHONY: all
