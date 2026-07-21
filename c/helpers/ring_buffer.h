#ifndef HELPERS_RING_BUFFER_H
#define HELPERS_RING_BUFFER_H

#include <stdbool.h>
#include <stdint.h>

#ifndef RING_BUFFER_INITIAL_CAPACITY
#   define RING_BUFFER_INITIAL_CAPACITY 16
#endif

typedef struct {
    void ** data;

    // the number of elements in the buffer.
    uint32_t length;

    // the size of the buffer.
    uint32_t capacity;

    // position of first element.
    uint32_t start;

    // position after the last element, where another element can be inserted.
    uint32_t next;
} ring_buffer_t;

static inline ring_buffer_t ring_buffer(void)
{
    return (ring_buffer_t){0};
}

void ring_buffer_free(ring_buffer_t * rb);

void ring_buffer_alloc(ring_buffer_t * rb, uint32_t count);

void ring_buffer_push(ring_buffer_t * rb, void * element);

// fatal error if called with [rb->length] == 0.
void * ring_buffer_pop(ring_buffer_t * rb);

#endif // HELPERS_RING_BUFFER_H
