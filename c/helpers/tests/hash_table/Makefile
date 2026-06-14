DIR = ~/AdventOfCode/c
HELPERS = $(addprefix $(DIR)/helpers/, hash_table.c)
CFLAGS = -g -Wall -Wextra -pedantic-errors -I $(DIR)

main: .FORCE
	$(CC) $(CFLAGS) hash_table.c $(DIR)/helpers.c $(HELPERS) ../unity/unity.c -o $@

run: main
	valgrind --leak-check=full -s ./main

.FORCE:
