#ifndef HELPERS_H
#define HELPERS_H

#include <stdint.h>
#include <stdlib.h>

// display formatted error message and append `errno` if message ends with ':'
// from dwm :p
void die(const char* fmt, ...);

// From:
// https://github.com/wren-lang
// http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2Float
uint32_t power_of_2_ceil(uint32_t n);

#endif // HELPERS_H
