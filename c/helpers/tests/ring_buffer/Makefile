DIR = ../..
HELPERS = $(addprefix $(DIR)/objs/, utils.o ring_buffer.o)

CFLAGS = -g -Wall -Wextra -pedantic-errors -fsanitize=signed-integer-overflow -I $(DIR)

all: helpers main

main: main.c
	$(CC) $(CFLAGS) main.c $(HELPERS) ../unity/unity.c -o $@

.PHONY: helpers
helpers:
	$(MAKE) -C $(DIR)
