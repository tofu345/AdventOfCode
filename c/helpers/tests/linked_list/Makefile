DIR = ../../../
HELPERS = $(addprefix $(DIR)/helpers/, linked_list.c arena.c)
CFLAGS = -g -Og -Wall -Wextra -pedantic-errors -fsanitize=signed-integer-overflow -I $(DIR)

main: .FORCE
	$(CC) $(CFLAGS) main.c $(DIR)/helpers.c $(HELPERS) ../unity/unity.c -o $@

run: main
	valgrind --leak-check=full -s ./main

.FORCE:
