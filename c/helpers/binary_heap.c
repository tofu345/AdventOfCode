#include "binary_heap.h"
#include "utils.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

static void heap_allocate(binary_heap_t* heap, uint32_t count);
static bool less(binary_heap_t* heap, uint32_t a, uint32_t b);
static void swim(binary_heap_t* heap, uint32_t index);
static void sink(binary_heap_t* heap, uint32_t index);
static bool remove_at(binary_heap_t* heap, uint32_t index, void** dest);

binary_heap_t binary_heap(compare_function_t* compare_function)
{
    return (binary_heap_t){ .__cmp = compare_function };
}

binary_heap_t binary_heap_of(uint32_t length, compare_function_t* compare_function)
{
    binary_heap_t heap = { .__cmp = compare_function };
    heap_allocate(&heap, length);
    return heap;
}

bool binary_heap_peek(binary_heap_t* heap, void** dest)
{
    if (heap->data == NULL || heap->length == 0)
        return false;

    assert(dest != NULL && "dest cannot be NULL");
    *dest = heap->data[0];
    return true;
}

bool binary_heap_pop(binary_heap_t* heap, void** dest)
{
    return remove_at(heap, 0, dest);
}

bool binary_heap_contains(binary_heap_t* heap, void* element)
{
    for (uint32_t i = 0; i < heap->length; i++)
        if (heap->data[i] == element) return true;
    return false;
}

void binary_heap_add(binary_heap_t* heap, void* element)
{
    heap_allocate(heap, 1);
    uint32_t index = heap->length++;
    heap->data[index] = element;
    swim(heap, index);
}

bool binary_heap_delete(binary_heap_t* heap, void* element)
{
    for (uint32_t i = 0; i < heap->length; i++)
    {
        if (heap->data[i] == element)
        {
            remove_at(heap, i, NULL);
            return true;
        }
    }

    return false;
}

void binary_heap_free(binary_heap_t* heap)
{
    free(heap->data);
    heap->data = NULL;
    heap->capacity = heap->length = 0;
}

bool _is_binary_heap(binary_heap_t* heap, uint32_t index)
{
    uint32_t len = heap->length;
    if (index >= len) return true;

    uint32_t left = 2 * index + 1,
         right = 2 * index + 2;

    if (left < len && !less(heap, index, left)) return false;
    if (right < len && !less(heap, index, right)) return false;

    return _is_binary_heap(heap, left) && _is_binary_heap(heap, right);
}

bool is_binary_heap(binary_heap_t* heap)
{
    return _is_binary_heap(heap, 0);
}

void _display(binary_heap_t* heap, display_fn* fn, uint32_t index, uint32_t depth)
{
    if (index >= heap->length) return;

    for (uint32_t i = 0; i < depth; i++) printf("  ");

    fn(heap->data[index]);

    uint32_t left = 2 * index + 1,
             right = 2 * index + 2;

    _display(heap, fn, left, depth+1);
    _display(heap, fn, right, depth+1);
}

void binary_heap_display(binary_heap_t* heap, display_fn* fn)
{
    _display(heap, fn, 0, 0);
}

// check if the value of node [a] <= node [b].
static bool less(binary_heap_t* heap, uint32_t a, uint32_t b)
{
    return heap->__cmp(heap->data[a], heap->data[b]);
}

static void swap(binary_heap_t* heap, uint32_t a, uint32_t b)
{
    void* tmp = heap->data[b];
    heap->data[b] = heap->data[a];
    heap->data[a] = tmp;
}

static void swim(binary_heap_t* heap, uint32_t index)
{
    uint32_t parent = (index - 1) / 2;

    while (index > 0 && less(heap, index, parent))
    {
        swap(heap, parent, index);
        index = parent;
        parent = (index - 1) / 2;
    }
}

static void sink(binary_heap_t* heap, uint32_t index)
{
    uint32_t len = heap->length;
    while (true)
    {
        uint32_t left = 2 * index + 1,
                 right = 2 * index + 2,
                 smallest = left;

        if (right < len && less(heap, right, left))
            smallest = right;

        if (left >= len || less(heap, index, smallest))
            break;

        swap(heap, smallest, index);
        index = smallest;
    }
}

static bool remove_at(binary_heap_t* heap, uint32_t index, void** dest)
{
    if (heap->data == NULL || heap->length == 0)
        return false;

    uint32_t last = --heap->length;
    swap(heap, index, last);

    if (index == last) 
    {
        if (dest != NULL)
            *dest = heap->data[last];
        return heap->data[last];
    }

    void* elem = heap->data[index];

    sink(heap, index);
    // If sinking did not work try swimming
    if (heap->data[index] == elem) swim(heap, index);

    if (dest != NULL)
        *dest = heap->data[last];
    return true;
}

static void heap_allocate(binary_heap_t* heap, uint32_t count)
{
    uint32_t new_length;
    if (__builtin_add_overflow(heap->length, count, &new_length))
        die("integer overflow adding %d elements to binary_heap", count);

    if (new_length > heap->capacity)
    {
        uint32_t capacity = power_of_2_ceil(new_length);
        void* data = realloc(heap->data, capacity * sizeof(void*));
        if (data == NULL) die("could not allocate %d more elements:", count);
        heap->data = data;
        heap->capacity = capacity;
    }
}
