CFLAGS = -g -Og -Wall -Wextra -pedantic-errors -fsanitize=signed-integer-overflow -I$(DIR)

DIR = ../../c
HELPERS = $(addprefix $(DIR)/helpers/, string_slice.c integer.c binary_heap.c \
	    hash_table.c arena.c)

main: .FORCE
	$(CC) $(CFLAGS) main.c $(DIR)/helpers.c $(HELPERS) -o $@

.FORCE:
