#include "ring_buffer.h"
#include "utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void ring_buffer_free(ring_buffer_t * buf)
{
    free(buf->data);
    *buf = (ring_buffer_t){0};
}

void ring_buffer_alloc(ring_buffer_t * buf, uint32_t count)
{
    uint32_t old_capacity = buf->capacity;
    buf->capacity = power_of_2_ceil(old_capacity + count);

    buf->data = realloc(buf->data, buf->capacity * sizeof(void *));
    if (buf->data == NULL) die("could not reallocate ring buffer\n");
    memset(buf->data + old_capacity, 0, old_capacity * sizeof(void *));

    if (buf->start != 0)
    {
        uint32_t from, to, length;
        if (buf->start > old_capacity / 2)
        {
            // update `buf->data`
            // from: { [0..end][start..old_capacity] ### }
            // to:   { [0..end] ### [start..capacity] }
            // where `###` is undefined data
            from = buf->start;
            to = buf->capacity - buf->start;
            length = old_capacity - buf->start;

            buf->start = to;
        }
        else
        {
            // update `buf->data`
            // from: { [0..end][start..old_capacity] ### }
            // to:   { ### [start..end] ### }
            // where `###` is undefined data
            from = 0;
            to = old_capacity;
            length = buf->start;

            buf->next = to + length;
        }

        // copy [from..from + length] to [to..to + length]
        for (uint32_t i = 0; i < length; from++, to++, i++)
        {
            buf->data[to] = buf->data[from];
        }
    }
}

void ring_buffer_push(ring_buffer_t * buf, void * element)
{
    if (buf->data == NULL) ring_buffer_alloc(buf, RING_BUFFER_INITIAL_CAPACITY);

    buf->length++;
    if (buf->next < buf->capacity)
    {
        // should never overwrite data at `buf->start`
        buf->data[buf->next++] = element;

        if (buf->next == buf->capacity && buf->start != 0)
            buf->next = 0;
        else if (buf->next == buf->start)
            buf->next = buf->capacity;

        return;
    }

    ring_buffer_alloc(buf, buf->capacity);
    buf->data[buf->next++] = element;
}

void * ring_buffer_pop(ring_buffer_t * buf)
{
    if (buf->data == NULL || buf->start == buf->next) die("no elements in ring buffer to pop");

    void * head = buf->data[buf->start];

    buf->length--;
    buf->start++;
    if (buf->start == buf->capacity)
        buf->start = 0;

    return head;
}
