TOP = $(shell pwd)
LIB = $(TOP)/lib
OUT ?= $(TOP)/out

export LIB
export OUT

all: | $(OUT)
	$(MAKE) -C posix

clean:
	rm -rf $(OUT)

$(OUT) :
	mkdir -p $@

.PHONY: all
