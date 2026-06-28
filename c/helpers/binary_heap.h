#ifndef HELPERS_BINARY_HEAP_H
#define HELPERS_BINARY_HEAP_H

// A binary heap implemented as an array.

// https://www.youtube.com/watch?v=wptevk0bshY
// https://github.com/williamfiset/Algorithms/blob/master/src/main/java/com/williamfiset/algorithms/datastructures/priorityqueue/BinaryHeap.java

#include <stdbool.h>
#include <stdint.h>

// should return true if [a] is less than or equal to [b].
typedef bool compare_function_t(void* a, void* b);

typedef struct {
    void** data;
    uint32_t length, capacity;
    compare_function_t* __cmp; // don't touch this
} binary_heap_t;

binary_heap_t binary_heap(compare_function_t* compare_function);

binary_heap_t binary_heap_of(uint32_t length,
                             compare_function_t* compare_function);

void binary_heap_free(binary_heap_t* heap);

// Store the value with the lowest priority (the minimum in a min-heap or the
// maximum in a max-heap) in the heap in [dest] and return true.
// Return false if there is no data present.
bool binary_heap_peek(binary_heap_t* heap, void** dest);

// Pop the value with the lowest priority (the minimum in a min-heap or the
// maximum in a max-heap) in the heap, store it in [dest] if not NULL and
// return true.
// Return false if there is no data present.
bool binary_heap_pop(binary_heap_t* heap, void** dest);

bool binary_heap_contains(binary_heap_t* heap, void* element);

void binary_heap_add(binary_heap_t* heap, void* element);

// Remove [element] from the heap if it exists and return true.
bool binary_heap_delete(binary_heap_t* heap, void* element);

// should print [element] to stdout.
typedef void display_fn(void* element);

void binary_heap_display(binary_heap_t* heap, display_fn* display);

// Returns `true` if the heap satisfies the binary heap property.
bool is_binary_heap(binary_heap_t* heap);

#endif // HELPERS_BINARY_HEAP_H
