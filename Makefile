CC = gcc
OUTPUT_OPTION = -MMD -MP -o $@
CFLAGS = -std=gnu11 -O -m32
LDFLAGS = -m32
LDLIBS = -lreadline -llua -lm -ldl

SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)
DEP = $(SRC:.c=.d)

all: luamon

luamon: luamon.o deux.o mnemonics.o | mnemonics.lua

deux.c: mnemonics.h

mnemonics.h mnemonics.c mnemonics.lua: mnemonics.py
	python mnemonics.py

-include $(DEP)

.PHONY: test debug debug_do clean

clean:
	rm -f $(OBJ) $(DEP) luamon runtest mnemonics.c mnemonics.h mnemonics.lua


debug:
	$(MAKE) -B debug_do

debug_do: CFLAGS += -g -O0
debug_do: all
