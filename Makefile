CC ?= gcc
OUT := libvorbis
SUFFIX := so
CFLAGS := -O3 -ftree-vectorize -fPIC -g
SSE_DEFAULT := -msse -mfpmath=sse
SSE_MAC_SILICON := -mavx2 -march=native
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
    ifeq ($(UNAME_S),Linux)
        OUT := $(OUT)-lin
	CFLAGS += -static-libgcc -include glibc-2.13.h
    endif
    ifeq ($(UNAME_S),Darwin)
        OUT := $(OUT)-mac
	SUFFIX = dylib
    endif
    PROC_P := $(shell $(CC) -dumpmachine)
    ifneq ($(filter, %x86_64,$(PROC_P)),)
	CFLAGS := $(CFLAGS) $(SSE_DEFAULT)
        OUT := $(OUT)-amd64
    endif
    ifneq ($(filter %86,$(PROC_P)),)
	CFLAGS := $(CFLAGS) $(SSE_DEFAULT)
        OUT := $(OUT)-i686
    endif
    ifneq ($(filter arm%,$(PROC_P)),)
	CFLAGS := $(CFLAGS) $(SSE_MAC_SILICON)
        OUT := $(OUT)-arm
    endif
endif

all:
	$(CC) -shared -o static/$(OUT).$(SUFFIX) $(CFLAGS) stb_vorbis_patch.c $(LDFLAGS)
