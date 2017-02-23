CC = gcc
OUTPUT_OPTION = -MMD -MP -o $@
CFLAGS = -std=gnu11 -O
LDLIBS = -lreadline -llua -lm -ldl

SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)
DEP = $(SRC:.c=.d)

all: alua

alua: alua.o deux.o mnemonics.o

deux.c: mnemonics.h

mnemonics.h mnemonics.c: mnemonics.py
	python mnemonics.py

-include $(DEP)

runtest: runtest.o deux.o mnemonics.o mt19937ar.o

test: runtest
	./runtest

.PHONY: test debug debug_do clean

clean:
	rm -f $(OBJ) $(DEP) alua runtest mnemonics.c mnemonics.h


debug:
	$(MAKE) -B debug_do

debug_do: CFLAGS += -g -O0
debug_do: all
