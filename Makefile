CFLAGS = -std=gnu11 -O -lreadline

all: stack

.PHONY: debug debug_do clean

debug:
	$(MAKE) -B debug_do

debug_do: CFLAGS += -g
debug_do: all

clean:
	rm stack
