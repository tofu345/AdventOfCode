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

#include "helpers.h"

// Turns out one can just use regexes
// Clearly, I am rusty

const char* filename = "input.txt";

static long part_one(long lo, long hi);
static void part_two(long lo, long hi, int n, long_buffer_t*);

// [https://www.geeksforgeeks.org/dsa/program-count-digits-integer-3-different-methods/]
static int num_digits(long n);

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
    long_buffer_t seen = long_buffer();

    for (char* cur = data; *cur != '\0'; cur++)
    {
        long lo = strtol(cur, NULL, 10);
        if (errno == ERANGE) die("integer out of range: %s", cur);

        cur = strchr(cur, '-');
        if (cur == NULL) die("invalid data: missing '-' seperator");
        cur++; // skip '-'

        long hi = strtol(cur, NULL, 10);
        if (errno == ERANGE) die("integer out of range: %s", cur);

        p1 += part_one(lo, hi);

        int num_hi_digits = num_digits(hi); // this is calculated twice, oh well.
        for (int i = 1; i < num_hi_digits; i++)
        {
            part_two(lo, hi, i, &seen);
        }
        for (uint32_t i = 0; i < seen.length; i++)
        {
            p2 += seen.data[i];
        }
        seen.length = 0;

        cur = strchr(cur, ',');
        if (cur == NULL) break;
    }

    printf("Part One: %ld\n", p1);
    printf("Part Two: %ld\n", p2);

    long_buffer_free(&seen);
    if (munmap(data, len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}

static int num_digits(long n)
{
    if (n == 0) return 1;

    // long count = 0;
    // while (n != 0)
    // {
    //     n /= 10;
    //     count++;
    // }
    // return count;

    // unsure if this is better

    return floor(log10(labs(n))) + 1;
}

static long _pow(long a, long pow)
{
    long result = 1;
    for (long i = 0; i < pow; i++)
        result *= a;
    return result;
}

static long part_one(long lo, long hi)
{
    long result = 0;
    int num_hi_digits = num_digits(hi);

beninging:
    int num_lo_digits = num_digits(lo);
    if (num_lo_digits % 2 != 0) // must have even number of digits
    {
        //     from 999 -> 1000
        // 3 digits ^^^     ^^^ 3 zeros
        lo = _pow(10, num_lo_digits);
        if (lo > hi) return 0;
        num_lo_digits++;
    }

    long zero_padding = _pow(10, num_lo_digits / 2);

    // loop from first half of `lo`
    int cur = lo / zero_padding;
    // to first half of `hi` or `zero_padding - 1`, whichever is smaller
    int end = num_lo_digits == num_hi_digits
            ? hi / zero_padding
            : zero_padding - 1;
    for (; cur <= end; cur++)
    {
        long num = cur + (cur * zero_padding);

        if (num < lo)
            continue;
        else if (num > hi)
            break;
        else
            result += num;
    }

    // e.g. lo: 1234, hi: 123456
    // done 1234 to 9999, now 100000 to 123456
    if (num_lo_digits < num_hi_digits)
    {
        lo = _pow(10, num_lo_digits + 1); // next even number of digits
        goto beninging;
    }

    return result;
}

static bool contains(long_buffer_t* seen, long element)
{
    for (uint32_t i = 0; i < seen->length; i++)
    {
        if (seen->data[i] == element)
            return true;
    }
    return false;
}

static void part_two(long lo, long hi, int n, long_buffer_t* seen)
{
    long num_hi_digits = num_digits(hi);
    if (num_hi_digits < n) return;

beninging:
    long num_lo_digits = num_digits(lo);
    if (num_lo_digits % n != 0)
    {
        lo = _pow(10, num_lo_digits);
        num_lo_digits++;
        if (lo > hi) return;

        // pad zeros until `n` is a factor of `num_lo_digits`
        while (num_lo_digits % n != 0)
        {
            num_lo_digits++;
            lo *= 10;
            if (lo > hi) return;
        }
    }

    long zero_padding = _pow(10, n);

    if (num_lo_digits == n)
    {
        // `lo` can only be repeated once with the current `n`
        // pad by `n` zeros to continue
        lo *= zero_padding;
        num_lo_digits += n;
        if (lo > hi) return;
    }

    // n: 2
    // lo: 12345678
    // lo / x: 12
    long x = _pow(10, num_lo_digits - n);

    // loop from first `n` of `lo`
    long cur = lo / x;
    // to first `n` of `hi` or `zero_padding - 1`, whichever is smaller
    long end = num_lo_digits == num_hi_digits
             ? hi / x
             : zero_padding - 1;

    // number of times `cur` has to be repeated (i.e. 12 -> 1212 -> 121212)
    // to have the same number of digits as `lo`
    int repeats = num_lo_digits / n;
    for (; cur <= end; cur++)
    {
        long num = cur;
        for (int i = 1; i < repeats; i++)
        {
            num *= zero_padding;
            num += cur;
        }

        if (num < lo)
            continue;
        else if (num > hi)
            break;
        else
        {
            // a hashmap would be much better. but too lazy to write one rn :p
            if (!contains(seen, num))
                long_buffer_push(seen, num);
        }
    }

    // e.g. lo: 1234, hi: 123456
    // done 1234 to 9999, now 100000 to 123456
    if (num_lo_digits < num_hi_digits)
    {
        lo = _pow(10, num_lo_digits);
        goto beninging;
    }
}
