CC = gcc
OUTPUT_OPTION = -MMD -MP -o $@
CFLAGS = -std=gnu11 -O
LDLIBS = -lreadline

SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)
DEP = $(SRC:.c=.d)

all: al

al: al.o deux.o mnemonics.o

deux.c: mnemonics.h

mnemonics.h mnemonics.c: mnemonics.py
	python mnemonics.py

-include $(DEP)

.PHONY: debug debug_do clean

clean:
	rm -f $(OBJ) al


debug:
	$(MAKE) -B debug_do

debug_do: CFLAGS += -g -O0
debug_do: all
