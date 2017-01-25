CC = gcc
OUTPUT_OPTION = -MMD -MP -o $@
CFLAGS = -std=gnu11 -O
LDLIBS = -lreadline

SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)
DEP = $(SRC:.c=.d)

all: stack

stack: stack.o

-include $(DEP)


.PHONY: debug debug_do clean

clean:
	rm -f $(OBJ) stack


debug:
	$(MAKE) -B debug_do

debug_do: CFLAGS += -g -O0
debug_do: all


DEPS := $(COBJS:.o=.d)

-include $(DEPS)
