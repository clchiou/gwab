HASTEC ?= hastec

GWAB	:= $(OUT)/pack/gwab.js
FILES	:= manifest.json $(wildcard *.html) $(wildcard *.js)
FILES	:= $(addprefix $(OUT)/pack/, $(FILES))

MAIN := $(shell pwd)/Main.hs
HS_SRCS := $(wildcard *.hs) $(wildcard ../lib/*.hs)

ARGS := -outputdir $(OUT) -i$(LIB) -i$(shell pwd)

all: $(GWAB) $(FILES) | $(OUT)/pack

# XXX: Changing directory to $(OUT) before calling hastec is a hack for placing
# intermediate object files there rather than here (because hastec does not
# provide a option for this).
$(GWAB) : $(HS_SRCS) | $(OUT)/pack
	cd $(OUT) ; $(HASTEC) $(ARGS) --out=$@ --make $(MAIN)

$(FILES) : $(OUT)/pack/% : % | $(OUT)/pack
	cp -f $< $@

$(OUT)/pack :
	mkdir -p $@

.PHONY: all
