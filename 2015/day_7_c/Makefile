CC=gcc
CFLAGS=-g -Wall -Wextra -Werror -O2 -std=c99 -pedantic
FILES=main.c

all: main
	@ ./a.out

main: ${FILES} Makefile
	@ ${CC} ${CFLAGS} ${FILES}
