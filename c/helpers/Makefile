CFLAGS = -g -Wall -Wextra -pedantic-errors

SRCS = $(wildcard *.c)

OBJS = $(SRCS:%.c=objs/%.o)

all: $(OBJS)

$(OBJS): objs/%.o: %.c %.h utils.h utils.c | objs
	$(CC) $(CFLAGS) -c $< -o $@

objs:
	mkdir -p objs

.PHONY: clean
clean:
	rm -r objs
