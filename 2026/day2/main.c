#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers/integer.h"
#include "helpers/string_slice.h"

const char* filename = "input.txt";

// Copied this person, my original solution was similir, but this is SO much better
// I don't fully understand the math, but I think I have the general idea
// https://www.reddit.com/r/adventofcode/comments/1pbzqcx/comment/nvmrsl3/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

const uint64_t unique_prime_factors[][2] = {
    {1,  0}, // 1
    {2,  0}, // 2
    {3,  0}, // 3
    {2,  0}, // 4
    {5,  0}, // 5
    {2,  3}, // 6
    {7,  0}, // 7
    {2,  0}, // 8
    {3,  0}, // 9
    {2,  5}, // 10
    {11, 0}, // 11
    {2,  3}, // 12
    {13, 0}, // 13
};

const uint64_t pow_10[] = {
    1,           // 0
    10,          // 1
    100,         // 2
    1000,        // 3
    10000,       // 4
    100000,      // 5
    1000000,     // 6
    10000000,    // 7
    100000000,   // 8
    1000000000,  // 9
    10000000000, // 10
};

static uint64_t sum_invalid_ids(uint64_t start, uint64_t end, uint64_t len,
                                int digits)
{
    uint64_t f = (pow_10[len] - 1) / (pow_10[digits] - 1);
    uint64_t a = ceil(MAX(start, pow_10[len - 1]) / (double)f);
    uint64_t b = MIN(end, pow_10[len]) / f;
    if (b < a) return 0;
    // sum all numbers with [a..b] inclusive, multiply by f
    return f * ((b*b) - (a*a) + a + b) / 2;
}

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
    string_t data_string = string_of(len, data);
    while (data_string.len > 0)
    {
        string_t range_string = string_split(&data_string, ','),
                 num = string_split(&range_string, '-');
        uint64_t start = parse_long(num.data),
                 start_len = num.len;

        num = string_split(&range_string, '-');
        uint64_t end = parse_long(num.data),
                 end_len = num.len;

        for (uint64_t len = start_len; len <= end_len; len++)
        {
            if (len == 1) continue;
            const uint64_t* primes = unique_prime_factors[len - 1];
            for (int i = 0; i < 2; i++)
            {
                if (primes[i] == 0) continue;

                uint64_t digits = len / primes[i],
                         result = sum_invalid_ids(start, end, len, digits);

                p2 += result;
                if (primes[i] == 2)
                    p1 += result;
            }

            // correct for overcounting
            if (primes[0] != 0 && primes[1] != 0) {
                p2 -= sum_invalid_ids(start, end, len, 1);
            }
        }
    }

    printf("Part One: %ld\n", p1);
    printf("Part Two: %ld\n", p2);

    if (munmap((void*)data, len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}
