DIR = ~/AdventOfCode/c
HELPERS = $(addprefix $(DIR)/helpers/, )
CFLAGS = -g -Wall -Wextra -pedantic-errors -I $(DIR)

main: .FORCE
	$(CC) $(CFLAGS) main.c $(DIR)/helpers.c $(HELPERS) -o $@

.FORCE:
