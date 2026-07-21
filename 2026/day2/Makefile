DIR = ../../c/helpers
HELPERS = $(addprefix $(DIR)/objs/, utils.o integer.o string_slice.o)

CFLAGS = -g -Wall -Wextra -pedantic-errors -fsanitize=signed-integer-overflow -I $(DIR)

all: helpers main

main: main.c
	$(CC) $(CFLAGS) main.c $(HELPERS) -o $@ -lm

.PHONY: helpers
helpers:
	$(MAKE) -C $(DIR)
