#include "arena.h"

#include <assert.h>
#include <sys/mman.h>
#include <unistd.h>

arena_region_t * arena_region(size_t capacity)
{
    size_t bytes = sizeof(arena_region_t) + (sizeof(uintptr_t) * capacity);
    arena_region_t *r =
        mmap(NULL, bytes, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    assert(r != MAP_FAILED);
    r->next = NULL;
    r->capacity = capacity;
    r->count = 0;
    return r;
}

void arena_region_free(arena_region_t * r)
{
    size_t bytes = sizeof(arena_region_t) + (sizeof(uintptr_t) * r->capacity);
    int ret = munmap(r, bytes);
    assert(ret == 0);
}

void * arena_alloc(arena_t * a, size_t bytes)
{
    size_t size = (bytes + sizeof(uintptr_t) - 1) / sizeof(uintptr_t);

    if (a->end == NULL)
    {
        assert(a->begin == NULL);
        size_t capacity = ARENA_REGION_DEFAULT_CAPACITY;
        if (capacity < size) capacity = size;
        a->end = arena_region(capacity);
        a->begin = a->end;
    }

    while (a->end->count + size > a->end->capacity && a->end->next != NULL)
    {
        a->end = a->end->next;
    }

    if (a->end->count + size > a->end->capacity)
    {
        assert(a->end->next == NULL);
        size_t capacity = ARENA_REGION_DEFAULT_CAPACITY;
        if (capacity < size) capacity = size;
        a->end->next = arena_region(capacity);
        a->end = a->end->next;
    }

    void * result = &a->end->data[a->end->count];
    a->end->count += size;
    return result;
}

void arena_free(arena_t * a)
{
    arena_region_t * r = a->begin;
    while (r != NULL)
    {
        arena_region_t * r0 = r;
        r = r->next;
        arena_region_free(r0);
    }
    a->begin = NULL;
    a->end = NULL;
}
