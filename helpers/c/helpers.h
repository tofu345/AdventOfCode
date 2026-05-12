#ifndef HELPERS_H
#define HELPERS_H

#include <stdint.h>

// display formatted error message and append `errno` if message ends with ':'
// from dwm :p
void die(const char* fmt, ...);

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
        int idx = buf->length++;                                              \
        if (buf->length == 0)                                                 \
        {                                                                     \
			die("integer overflow adding an element to %s_buffer_t", #name);  \
        }                                                                     \
        if (buf->length > buf->capacity)                                      \
        {                                                                     \
            uint32_t capacity = power_of_2_ceil(buf->length);                 \
            buf->data = realloc(buf->data, capacity * sizeof(typ));           \
            buf->capacity = capacity;                                         \
        }                                                                     \
        buf->data[idx] = val;                                                 \
    }                                                                         \
                                                                              \
    void name##_buffer_free(name##_buffer_t* buf) {                           \
        free(buf->data);                                                      \
        *buf = (name##_buffer_t){0};                                          \
    }

DECLARE_BUFFER(int, int)
DECLARE_BUFFER(long, long)

uint32_t power_of_2_ceil(uint32_t n);

#endif // HELPERS_H
