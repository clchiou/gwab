HASTEC ?= hastec

CURDIR	:= $(shell pwd)

GWAB	:= $(OUT)/pack/gwab.js
FILES	:= manifest.json \
	   launch.js \
	   jquery-2.0.3.min.js jquery-2.0.3.min.map \
	   $(wildcard *.html)
FILES	:= $(addprefix $(OUT)/pack/, $(FILES))

MAIN	:= $(CURDIR)/Main.hs
JS_SRCS	:= $(CURDIR)/wrapper.js
HS_SRCS	:= $(wildcard *.hs) $(wildcard ../lib/*.hs)

ARGS := -outputdir $(OUT) -i$(LIB) -i$(CURDIR) --debug

all: $(GWAB) $(FILES) | $(OUT)/pack

# XXX: Changing directory to $(OUT) before calling hastec is a hack for placing
# intermediate object files there rather than here (because hastec does not
# provide a option for this).
$(GWAB) : $(JS_SRCS) $(HS_SRCS) | $(OUT)/pack
	cd $(OUT) ; \
	$(HASTEC) $(ARGS) --out=$@ --with-js=$(JS_SRCS) --make $(MAIN)

$(FILES) : $(OUT)/pack/% : % | $(OUT)/pack
	cp -f $< $@

$(OUT)/pack :
	mkdir -p $@

.PHONY: all
