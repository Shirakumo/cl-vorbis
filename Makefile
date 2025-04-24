CC ?= gcc
OUT := libvorbis
SUFFIX := so
CFLAGS := -O3 -ftree-vectorize -fPIC -g
SSE_DEFAULT := -msse -mfpmath=sse
SSE_NATIVE := -march=native
LDFLAGS := -l m

ifeq ($(OS),Windows_NT)
    CFLAGS := $(CFLAGS) $(SSE_DEFAULT)
    OUT := $(OUT)-win
    SUFFIX := dll
    ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
        OUT := $(OUT)-amd64
    else
        ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
            OUT := $(OUT)-amd64
        endif
        ifeq ($(PROCESSOR_ARCHITECTURE),x86)
            OUT := $(OUT)-i686
        endif
    endif
else
    UNAME_S := $(shell uname -s)
	UNAME_M := $(shell uname -m)
    ifeq ($(UNAME_S),Linux)
        OUT := $(OUT)-lin
		ifeq ($(UNAME_M),x86_64)
			CFLAGS += -static-libgcc -include glibc-2.13.h
		endif
    endif
    ifeq ($(UNAME_S),Darwin)
        OUT := $(OUT)-mac
		SUFFIX = dylib
    endif
    ifeq ($(UNAME_M),x86_64)
		CFLAGS += $(SSE_DEFAULT)
        OUT := $(OUT)-amd64
    endif
    ifneq ($(filter %86,$(UNAME_M)),)
		CFLAGS += $(SSE_DEFAULT)
        OUT := $(OUT)-i686
    endif
    ifneq ($(filter aarch64%,$(UNAME_M)),)
		CFLAGS += $(SSE_NATIVE)
        OUT := $(OUT)-arm64
    endif
endif

all:
	$(CC) -shared -o static/$(OUT).$(SUFFIX) $(CFLAGS) stb_vorbis_patch.c $(LDFLAGS)
