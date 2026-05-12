#include <fcntl.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers.h"

// deceptively hard, or maybe I just suck..
// https://aoc.just2good.co.uk/2025/1.html

#define DIAL_NUMS 100

const char* filename = "input.txt";

// modulus with floored division [https://www.geeksforgeeks.org/c/modulus-on-negative-numbers/]
static inline int modulus(int a, int b)
{
    return a - b * floor((float)a / b);
}

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t len = lseek(fd, 0, SEEK_END);
    if (len == -1) die("could not get file length:");

    char* mapping = mmap(0, len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (mapping == MAP_FAILED) die("could not perform mmap:");

    int curr_pos = 50;
    int passwd1 = 0;
    int passwd2 = 0;

    char* data = mapping;
    for (; *data != 0; data++)
    {
        int rotation = strtol(data + 1, NULL, 10);
        int new_pos;
        char dir = *data;
        if (dir == 'L')
        {
            new_pos = modulus(curr_pos - rotation, DIAL_NUMS);

            if (curr_pos == 0)
            {
                // count wrap arounds, not current 0.
                passwd2 += rotation / DIAL_NUMS;
            }
            else if (rotation > curr_pos)
            {
                // 0 is crossed at least once.
                passwd2 += ((rotation - curr_pos - 1) / DIAL_NUMS) + 1;

                // count if end on 0.
                if (new_pos == 0)
                    passwd2++;
            }
            else if (rotation == curr_pos)
            {
                // end exactly on 0.
                passwd2++;
            }
        }
        else if (dir == 'R')
        {
            new_pos = modulus(curr_pos + rotation, DIAL_NUMS);
            // count wrap arounds.
            passwd2 += (curr_pos + rotation) / DIAL_NUMS;
        }
        else
            die("invalid data: %s");

        if (new_pos == 0)
            passwd1++;

        curr_pos = new_pos;

        // printf("%c%d\t%d:\t%d\n", dir, rotation, curr_pos, passwd2);

        data = strchr(data, '\n');
        if (data == NULL) break;
    }

    printf("Part One: %d\n", passwd1);
    printf("Part Two: %d\n", passwd2);

    if (munmap(mapping, len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}
