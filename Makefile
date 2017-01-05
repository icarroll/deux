all: stack

stack: stack.c
	gcc -std=gnu11 -Ofast -o stack stack.c
