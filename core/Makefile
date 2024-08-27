CC	   := clang
CFLAGS := -pipe -xassembler-with-cpp

ifeq ($(OS),Windows_NT)
	ifeq ($(shell gcc -dumpmachine),mingw32)
		CAT := type
		ABI := sysv
		ARCH := $(shell echo %PROCESSOR_ARCHITECTURE% | tr A-Z a-z)
	else
		$(error On Windows, only MinGW is supported. Please use MinGW GCC.)
	endif
else
	CAT := cat
	ARCH := $(shell uname -m)
	ifeq ($(shell uname -s),Darwin)
		ABI := apple
	else
		ABI := sysv
	endif
endif


ifeq ($(ARCH),x86_64)
	ARCH := amd64
endif
ifeq ($(ARCH),aarch64)
	ARCH := arm64
endif
ifeq ($(ARCH),riscv64)
	ARCH := rv64
endif


CORE_FILES   := $(wildcard *.ab)
CORE_OBJECTS := $(CORE_FILES:.ab=.o)


all: _build/scalls.o $(CORE_OBJECTS)

_build:
	mkdir -p _build

_build/scalls.o: _build scalls/$(ARCH)-header.s scalls/$(ARCH)_$(ABI).s
	$(CAT) scalls/$(ARCH)-header.s scalls/$(ARCH)_$(ABI).s | $(CC) -c $(CFLAGS) -o $@ -

%.o: %.ab
	# TODO: implement a -c flag in aback
	aback -a $^ | $(CC) -c $(CFLAGS) -o _build/$@ -

clean:
	rm -rf _build


.PHONY: all clean
