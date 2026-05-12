#include <fcntl.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers.h"

const char* filename = "input.txt";

long max_joltage(const char* data, int length, int num_batteries);

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t len = lseek(fd, 0, SEEK_END);
    if (len == -1) die("could not get file length:");

    const char* data = mmap(0, len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) die("could not perform mmap:");

    long p1 = 0;
    long p2 = 0;
    const char* cur = data;
    while (*cur != 0)
    {
        const char* end = strpbrk(cur, "\n"); // '\n' or '\0'
        if (end == NULL) die("invalid data");

        int length = end - cur;

        p1 += max_joltage(cur, length, 2);
        p2 += max_joltage(cur, length, 12);

        cur = end + 1;
    }

    printf("Part One: %ld\n", p1);
    printf("Part Two: %ld\n", p2);

    if (munmap((void*)data, len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}

long max_joltage(const char* data, int length, int num_batteries)
{
    long joltage = 0;

    int idx = -1; // index of the previous battery, all other batteries must follow it
    // make sure there are always at least enough batteries that follow `idx`
    for (int bat = num_batteries; bat >= 1; bat--)
    {
        char max = 0;
        for (int i = idx + 1; i < (length - bat); i++)
        {
            if (data[i] > max)
            {
                max = data[i];
                idx = i;
            }
        }

        joltage *= 10;
        joltage += (max - 48);
    }

    return joltage;
}
