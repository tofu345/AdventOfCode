INCS = -I ~/AdventOfCode/helpers/c
CFLAGS = -g -Wall -Wextra -std=c23 -pedantic-errors $(INCS)

main: main.c ~/AdventOfCode/helpers/c/*.c
	$(CC) $(CFLAGS) $^ -o $@ -lm
