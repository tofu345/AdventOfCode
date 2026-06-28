DIR = ../../../
HELPERS = $(addprefix $(DIR)/helpers/, string_slice.c)
CFLAGS = -g -Og -Wall -Wextra -pedantic-errors -fsanitize=signed-integer-overflow -I $(DIR)

main: .FORCE
	$(CC) $(CFLAGS) main.c $(DIR)/helpers.c $(HELPERS) ../unity/unity.c -o $@

run: main
	valgrind --leak-check=full -s ./main

.FORCE:
