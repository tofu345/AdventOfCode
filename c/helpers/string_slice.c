#include "string_slice.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>

void string_print(string_t s)
{
    for (size_t i = 0; i < s.len; i++)
    {
        putchar(s.data[i]);
    }
}

bool string_equal(string_t a, string_t b)
{
    if (a.len != b.len) return false;
    for (size_t i = 0; i < a.len; i++)
    {
        if (a.data[i] != b.data[i])
            return false;
    }
    return true;
}

bool string_equal_cstring(string_t a, char* b)
{
    if (a.len == 0 && *b == '\0') return true;
    for (size_t i = 0; i < a.len; i++)
    {
        if (b[i] == '\0' || a.data[i] != b[i])
            return false;
    }
    return true;
}

bool string_starts_with(string_t s, string_t prefix)
{
    if (prefix.len > s.len) return false;
    for (size_t i = 0; i < prefix.len; i++)
    {
        if (s.data[i] != prefix.data[i])
            return false;
    }
    return true;
}

string_t string_take(string_t s, size_t n)
{
    if (n > s.len) n = s.len;
    return (string_t){ s.data, n };
}

string_t string_drop(string_t s, size_t n)
{
    if (n > s.len) n = s.len;
    return (string_t){ s.data + n, s.len - n };
}

string_t string_trim(string_t s)
{
    while (s.len > 0 && isspace((unsigned char)s.data[0]))
    {
        s.data += 1;
        s.len -= 1;
    }

    while (s.len > 0 && isspace((unsigned char)s.data[s.len - 1]))
    {
        s.len -= 1;
    }

    return s;
}

string_t string_split(string_t *s, char delimeter)
{
    size_t i = 0;
    while (i < s->len && s->data[i] != delimeter)
        i++;

    string_t result = string_take(*s, i);
    if (i < s->len)
        *s = string_drop(*s, i + 1);
    else
        *s = string_drop(*s, i);

    return result;
}
