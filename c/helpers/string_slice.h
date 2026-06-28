#ifndef HELPERS_STRING_SLICE_H
#define HELPERS_STRING_SLICE_H

// based on: https://www.youtube.com/watch?v=fUVvfDkDb-Y

#include <stddef.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    const char* data;
    size_t len;
} string_t;

static inline string_t string(const char* cstring)
{
    return (string_t){
        .data = cstring,
        .len = strlen(cstring),
    };
}

static inline string_t string_of(size_t len, const char* cstring)
{
    return (string_t){
        .data = cstring,
        .len = len,
    };
}

void string_print(string_t s);
bool string_equal(string_t a, string_t b);
bool string_equal_cstring(string_t a, char* b);
bool string_starts_with(string_t s, string_t prefix);
string_t string_take(string_t s, size_t n);
string_t string_drop(string_t s, size_t n);
string_t string_trim(string_t s);

// return the first split of [s] by [delimeter] and drop all characters in [s]
// to the [delimeter].
string_t string_split(string_t *s, char delimeter);

#endif // HELPERS_STRING_SLICE_H
