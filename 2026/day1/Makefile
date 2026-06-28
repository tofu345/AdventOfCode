DIR = ../../c
CFLAGS = -g -Wall -Wextra -pedantic-errors -I $(DIR)

main: .FORCE
	$(CC) $(CFLAGS) main.c $(DIR)/helpers.c -o $@ -lm

.FORCE:
