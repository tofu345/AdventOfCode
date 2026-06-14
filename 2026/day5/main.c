#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers.h"

const char* filename = "input.txt";

typedef struct {
    long lo;
    long hi;
} range_t;

DECLARE_BUFFER(range, range_t)
DEFINE_BUFFER(range, range_t)

static inline
bool in_range(range_t range, long id)
{
    return id >= range.lo && id <= range.hi;
}

static inline
bool overlap(range_t a, range_t b)
{
    return in_range(a, b.lo) || in_range(a, b.hi);
}

static inline
range_t combine(range_t a, range_t b)
{
    return (range_t){ MIN(a.lo, b.lo), MAX(a.hi, b.hi) };
}

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t len = lseek(fd, 0, SEEK_END);
    if (len == -1) die("could not get file length:");

    const char* data = mmap(0, len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) die("could not perform mmap:");

    range_buffer_t ranges = range_buffer();

    const char* cur = data;
    const char* end = strstr(cur, "\n\n");
    if (end == NULL) die("invalid data");

    for (;; cur++)
    {
        range_t r = {0};
        r.lo = strtol(cur, NULL, 10);
        if (errno == ERANGE) die("integer out of range: %s", cur);

        cur = strchr(cur, '-');
        if (cur == NULL) die("invalid data: missing '-' seperator");
        cur++; // skip '-'

        r.hi = strtol(cur, NULL, 10);
        if (errno == ERANGE) die("integer out of range: %s", cur);

        range_buffer_push(&ranges, r);

        cur = strchr(cur, '\n');
        if (cur == NULL) die("invalid data");

        if (cur == end) break;
    }
    cur += 2; // skip '\n\n'

    // brute force combining ranges that can be combined
    while (1)
    {
        bool combine_occured = false;
        for (uint32_t i = 0; i < ranges.length; i++)
        {
            for (uint32_t j = 0; j < ranges.length; j++)
            {
                if (j == i) continue;

                range_t a = ranges.data[i],
                        b = ranges.data[j];

                if (overlap(a, b))
                {
                    ranges.data[i] = combine(a, b);
                    ranges.data[j] = ranges.data[ranges.length - 1];
                    ranges.length--;
                    combine_occured = true;
                }
            }
        }
        if (combine_occured == false) break;
    }

    int p1 = 0;
    for (; *cur != '\0'; cur++)
    {
        long id = strtol(cur, NULL, 10);
        if (errno == ERANGE) die("integer out of range: %s", cur);

        for (uint32_t i = 0; i < ranges.length; i++)
        {
            if (in_range(ranges.data[i], id))
            {
                p1++;
                break;
            }
        }

        cur = strchr(cur, '\n');
        if (cur == NULL) die("invalid data");
    }

    long p2 = 0;
    for (uint32_t i = 0; i < ranges.length; i++)
    {
        p2 += ranges.data[i].hi - ranges.data[i].lo + 1; // +1 because range is inclusive
    }

    printf("Part One: %d\n", p1);
    printf("Part Two: %ld\n", p2);

    range_buffer_free(&ranges);
    if (munmap((void*)data, len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}
