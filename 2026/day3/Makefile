DIR = ../../c
HELPERS = $(addprefix $(DIR)/helpers/, string_slice.c)
CFLAGS = -g -Wall -Wextra -pedantic-errors -I $(DIR)

main: .FORCE
	$(CC) $(CFLAGS) main.c $(DIR)/helpers.c $(HELPERS) -o $@

.FORCE:
