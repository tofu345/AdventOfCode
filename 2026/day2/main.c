#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers/integer.h"

const char* filename = "input.txt";

// One can just use regexes
// but I decided to be a bit 'smart', heh
// instead of looping through all numbers in the range and finding which have repeated sequences of digits
// i loop though the numbers with repeated sequences of digits within the range
// just read the code :P

// TODO: understand this solution, seems much shorter than the mess I did
// https://www.reddit.com/r/adventofcode/comments/1pbzqcx/comment/nvmrsl3/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

typedef struct {
    long start;
    long end;
    int start_n, end_n; // number of digits
} range_t;

static long repeated_twice(range_t);
static void repeated_n(range_t, int n, long_buffer_t* invalid_ids);

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t len = lseek(fd, 0, SEEK_END);
    if (len == -1) die("could not get file length:");

    char* data = mmap(0, len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) die("could not perform mmap:");

    long p1 = 0;
    long p2 = 0;
    range_t range;
    long_buffer_t ids = long_buffer();

    for (char* cur = data; *cur != '\n' && *cur != '\0'; cur++)
    {
        char* endptr;
        range.start = parse_long_(cur, &endptr);
        range.start_n = endptr - cur;

        if (*endptr != '-') die("invalid data: missing '-'");
        cur = endptr + 1;

        range.end = parse_long_(cur, &endptr);
        range.end_n = endptr - cur;

        if (*endptr != ',' && !(*endptr == '\n' || *endptr == '\0'))
            die("invalid data: missing ','");
        cur = endptr;

        p1 += repeated_twice(range);

        for (int n = 1; n <= range.end_n / 2; n++)
            repeated_n(range, n, &ids);

        for (uint32_t i = 0; i < ids.length; i++)
            p2 += ids.data[i];

        ids.length = 0;
    }

    printf("Part One: %ld\n", p1);
    printf("Part Two: %ld\n", p2);

    long_buffer_free(&ids);
    if (munmap(data, len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}

static long _pow(long a, long pow)
{
    // using <math.h> `pow()` may be better, but idk
    long result = 1;
    for (long i = 0; i < pow; i++)
        result *= a;
    return result;
}

static long repeated_twice(range_t range)
{
    long result = 0;
    long start = range.start;
    int start_n = range.start_n;

beninging:
    if (start_n % 2 != 0) // must have even number of digits
    {
        //     from 999 -> 1000
        // 3 digits ^^^     ^^^ 3 zeros
        start = _pow(10, start_n);
        start_n++;
        if (start > range.end) return result;
    }

    long zero_padding = _pow(10, start_n / 2);

    // loop from first half of `start`
    int i = start / zero_padding;
    // to first half of `range.end` or `zero_padding - 1`, whichever is smaller
    int j = start_n == range.end_n
          ? range.end / zero_padding
          : zero_padding - 1;

    for (; i <= j; i++)
    {
        long id = i + (i * zero_padding);

        if (id < start)
            continue;
        else if (id > range.end)
            break;
        else
            result += id;
    }

    // e.g. start: 1234, range.end: 123456
    // done 1234 to 9999, now 100000 to 123456
    if (start_n < range.end_n)
    {
        start = _pow(10, start_n + 1);
        start_n += 2;
        if (start > range.end) return result;
        goto beninging;
    }

    return result;
}

static bool contains(long_buffer_t* ids, long element)
{
    for (uint32_t i = 0; i < ids->length; i++)
    {
        if (ids->data[i] == element)
            return true;
    }
    return false;
}

static void repeated_n(range_t range, int n, long_buffer_t* ids)
{
    if (range.end_n < n) return;

    long start = range.start;
    int start_n = range.start_n;

beninging:
    if (start_n == n || start_n % n != 0)
    {
        start = _pow(10, start_n);
        start_n++;
        if (start > range.end) return;

        // pad zeros until `n` is a factor of `start_n`
        while (start_n % n != 0)
        {
            start *= 10;
            start_n++;
            if (start > range.end) return;
        }
    }

    // number of times to be repeated (i.e. 12 -> 1212 -> 121212)
    // to have the same number of digits as `start`
    int repeats = start_n / n;
    long zero_padding = _pow(10, n);

    // dividing `start` by `x` leaves the first `n` digits
    long x = _pow(10, start_n - n);

    // loop from first `n` of `start`
    long i = start / x;
    // to first `n` of `range.end` or `zero_padding - 1`, whichever is smaller
    long j = start_n == range.end_n
           ? range.end / x
           : zero_padding - 1;

    for (; i <= j; i++)
    {
        // repeat `i` `n` times
        long id = i;
        for (int k = 1; k < repeats; k++)
        {
            id *= zero_padding;
            id += i;
        }

        if (id < start)
            continue;
        else if (id > range.end)
            break;
        else
        {
            // a hashmap would be much better. but too lazy to write/find one rn :p
            if (!contains(ids, id))
                long_buffer_push(ids, id);
        }
    }

    // e.g. start: 1234, range.end: 123456
    // done 1234 to 9999, now 100000 to 123456
    if (start_n < range.end_n)
    {
        start = _pow(10, start_n);
        start_n++;
        if (start > range.end) return;
        goto beninging;
    }
}
