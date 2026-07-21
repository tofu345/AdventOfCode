DIR = ../../c/helpers
HELPERS = $(addprefix $(DIR)/objs/, utils.o string_slice.o integer.o \
	binary_heap.o hash_table.o arena.o)

CFLAGS = -g -Wall -Wextra -pedantic-errors -fsanitize=signed-integer-overflow -I $(DIR)

all: helpers main

main: main.c
	$(CC) $(CFLAGS) main.c $(HELPERS) -o $@

.PHONY: helpers
helpers:
	$(MAKE) -C $(DIR)
FORCE:
