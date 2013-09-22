TOP = $(shell pwd)
LIB = $(TOP)/lib
OUT ?= $(TOP)/out

export LIB

all: | $(OUT)/posix $(OUT)/chrome
	OUT=$(OUT)/posix $(MAKE) -C posix
	OUT=$(OUT)/chrome $(MAKE) -C chrome

clean:
	rm -rf $(OUT)

$(OUT)/posix $(OUT)/chrome :
	mkdir -p $@

.PHONY: all
