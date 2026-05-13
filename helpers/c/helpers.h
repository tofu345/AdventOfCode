#ifndef HELPERS_H
#define HELPERS_H

#include <stdint.h>
#include <stdlib.h>

typedef struct {
    int x;
    int y;
} point_t;

static inline point_t add_point(point_t a, point_t b)
{
    return (point_t){ a.x + b.x, a.y + b.y };
}

// from wren :p
// A poor man's generic for buffers of different types.
#define DECLARE_BUFFER(name, typ)                                             \
    typedef struct {                                                          \
        typ *data;                                                            \
        uint32_t length;                                                      \
        uint32_t capacity;                                                    \
    } name##_buffer_t;                                                        \
                                                                              \
    name##_buffer_t name##_buffer(void);                                      \
    void name##_buffer_fill(name##_buffer_t*, typ, int);                      \
    void name##_buffer_push(name##_buffer_t*, typ);                           \
    void name##_buffer_free(name##_buffer_t*);                                \

#define DEFINE_BUFFER(name, typ)                                              \
    name##_buffer_t name##_buffer(void)                                       \
    {                                                                         \
        return (name##_buffer_t){0};                                          \
    }                                                                         \
                                                                              \
    void name##_buffer_push(name##_buffer_t* buf, typ val)                    \
    {                                                                         \
        name##_buffer_fill(buf, val, 1);                                      \
    }                                                                         \
                                                                              \
    void name##_buffer_fill(name##_buffer_t* buf, typ val, int count)         \
    {                                                                         \
        uint32_t length;                                                      \
        if (__builtin_add_overflow(buf->length, count, &length)) {            \
        	die("integer overflow adding %d elements to %s_buffer_t",         \
                count, #name);                                                \
        }                                                                     \
        if (length > buf->capacity)                                           \
        {                                                                     \
            uint32_t capacity = power_of_2_ceil(length);                      \
            buf->data = realloc(buf->data, capacity * sizeof(typ));           \
            buf->capacity = capacity;                                         \
        }                                                                     \
        for (int i = 0; i < count; i++) {                                     \
            buf->data[buf->length++] = val;                                   \
        }                                                                     \
    }                                                                         \
                                                                              \
    void name##_buffer_free(name##_buffer_t* buf) {                           \
        free(buf->data);                                                      \
        *buf = (name##_buffer_t){0};                                          \
    }

DECLARE_BUFFER(int, int)
DECLARE_BUFFER(long, long)

uint32_t power_of_2_ceil(uint32_t n);

// display formatted error message and append `errno` if message ends with ':'
// from dwm :p
void die(const char* fmt, ...);

#endif // HELPERS_H
