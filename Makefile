TOP = $(shell pwd)
LIB = $(TOP)/lib
OUT ?= $(TOP)/out

export LIB

all: | $(OUT)/test $(OUT)/posix $(OUT)/chrome
	OUT=$(OUT)/test   $(MAKE) -C test
	OUT=$(OUT)/posix  $(MAKE) -C posix
	OUT=$(OUT)/chrome $(MAKE) -C chrome

clean:
	rm -rf $(OUT)

$(OUT)/test $(OUT)/posix $(OUT)/chrome :
	mkdir -p $@

.PHONY: all
