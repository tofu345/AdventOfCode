#ifndef HELPERS_INT_H
#define HELPERS_INT_H

#include <errno.h>

#include "helpers.h"

static inline long parse_long(const char* string)
{
    char* endptr;
    long num = strtol(string, &endptr, 10);
    if (errno == ERANGE) die("integer out of range: %s", string);
    else if (endptr == string) die("could not parse integer: %s", string);
    return num;
}

static inline long parse_long_(const char* string, char** endptr)
{
    long num = strtol(string, endptr, 10);
    if (errno == ERANGE) die("integer out of range: %s", string);
    else if (*endptr == string) die("could not parse integer: %s", string);
    return num;
}

DECLARE_BUFFER(int, int)
DECLARE_BUFFER(long, long)

#endif // HELPERS_INT_H
