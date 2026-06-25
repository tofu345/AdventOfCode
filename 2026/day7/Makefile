DIR = ~/AdventOfCode/c
HELPERS = $(addprefix $(DIR)/helpers/, integer.c)
CFLAGS = -Og -g -Wall -Wextra -pedantic-errors -fsanitize=signed-integer-overflow -I $(DIR)


main: .FORCE
	$(CC) $(CFLAGS) main.c $(DIR)/helpers.c $(HELPERS) -o $@

.FORCE:
