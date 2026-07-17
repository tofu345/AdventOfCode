#ifndef HELPERS_ARENA_H
#define HELPERS_ARENA_H

#include <stddef.h>
#include <stdint.h>

// copied from https://github.com/tsoding/arena

#ifndef ARENA_REGION_DEFAULT_CAPACITY
#   define ARENA_REGION_DEFAULT_CAPACITY (8*1024) // 8kb
#endif

typedef struct arena_region {
    struct arena_region * next;
    size_t capacity;
    size_t count;
    uintptr_t data[];
} arena_region_t;

typedef struct {
    arena_region_t * begin;
    arena_region_t * end;
} arena_t;

static inline arena_t arena_new(void)
{
    return (arena_t){ .begin = NULL, .end = NULL };
}

void * arena_alloc(arena_t * a, size_t bytes);
void arena_free(arena_t * a);

arena_region_t * arena_region(size_t capacity);
void arena_region_free(arena_region_t * r);

#endif // HELPERS_ARENA_H
