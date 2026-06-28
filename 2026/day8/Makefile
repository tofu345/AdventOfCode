DIR = ../../c
HELPERS = $(addprefix $(DIR)/helpers/, binary_heap.c integer.c string_slice.c)
CFLAGS = -g -Og -Wall -Wextra -pedantic-errors -fsanitize=signed-integer-overflow -I$(DIR)

main: .FORCE
	$(CC) $(CFLAGS) main.c $(DIR)/helpers.c $(HELPERS) -o $@ -lm

.FORCE:
