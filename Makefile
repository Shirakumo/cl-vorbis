CC ?= gcc
OUT := libvorbis
SUFFIX := so
CFLAGS := -O3 -ftree-vectorize -fPIC -g
SSE_DEFAULT := -msse -mfpmath=sse
SSE_NATIVE := -march=armv8-a+simd+crypto+crc
LDFLAGS := -l m
MACH := $(shell $(CC) -dumpmachine)
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
    ifneq (,$(findstring linux,$(MACH)))
        OUT := $(OUT)-lin
		ifneq (,$(findstring x86_64,$(MACH)))
			CFLAGS += -static-libgcc -include glibc-2.13.h
		endif
    else ifneq (,$(findstring darwin,$(MACH)))
        OUT := $(OUT)-mac
		SUFFIX = dylib
    endif
    ifneq (,$(findstring x86_64,$(MACH)))
		CFLAGS += $(SSE_DEFAULT)
        OUT := $(OUT)-amd64
    else ifneq (,$(findstring x86,$(MACH)))
		CFLAGS += $(SSE_DEFAULT)
        OUT := $(OUT)-i686
    else ifneq (,$(findstring aarch64,$(MACH)))
		CFLAGS += $(SSE_NATIVE)
        OUT := $(OUT)-arm64
    endif
endif

all:
	$(CC) -shared -o static/$(OUT).$(SUFFIX) $(CFLAGS) stb_vorbis_patch.c $(LDFLAGS)
