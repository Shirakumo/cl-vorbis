CC ?= gcc
OUT := libvorbis
SUFFIX := so

ifeq ($(OS),Windows_NT)
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
    endif
    ifeq ($(UNAME_S),Darwin)
        OUT := $(OUT)-mac
	SUFFIX = dylib
    endif
    PROC_P := $(shell $(CC) -dumpmachine)
    ifeq ($(filter, %x86_64,$(PROC)),)
        OUT := $(OUT)-amd64
    endif
    ifneq ($(filter %86,$(PROC)),)
        OUT := $(OUT)-i686
    endif
    ifneq ($(filter arm%,$(PROC)),)
        OUT := $(OUT)-arm
    endif
endif

all:
	$(CC) -O3 -ftree-vectorize -msse4.2 -mfpmath=sse -shared -o static/$(OUT).$(SUFFIX) -fPIC -static-libgcc -include glibc-2.13.h stb_vorbis.c -l m
