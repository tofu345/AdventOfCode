#include <fcntl.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers.h"
#include "helpers/binary_heap.h"
#include "helpers/integer.h"
#include "helpers/string_slice.h"

// not my most memory efficient code, this

const char* filename = "input.txt";

typedef struct {
    int x, y, z;
    int circuit;
} box_t;

typedef struct {
    uint32_t box_a, box_b;
    double distance;
} pair_t;

// should have just used c++ lol
DECLARE_BUFFER(box, box_t)
DEFINE_BUFFER(box, box_t)
DECLARE_BUFFER(pair, pair_t)
DEFINE_BUFFER(pair, pair_t)
DECLARE_BUFFER(uint, uint32_t)
DEFINE_BUFFER(uint, uint32_t)

static inline double straight_line_distance(box_t a, box_t b)
{
    long x = a.x - b.x,
         y = a.y - b.y,
         z = a.z - b.z;
    return sqrt(x*x + y*y + z*z);
}

pair_buffer_t pairs; // hack

static bool smaller_pair(void* a, void* b)
{
    return pairs.data[(uintptr_t)a].distance
        <= pairs.data[(uintptr_t)b].distance;
}

static bool bigger_integer(void* a, void* b)
{
    return (intptr_t)a > (intptr_t)b;
}

static void connect(box_t* a, box_t* b, uint_buffer_t* circuits,
                    const box_buffer_t* boxes)
{
    if (a->circuit == 0 && b->circuit == 0)
    {
        // both [a] and [b] are not connected to other boxes
        a->circuit = b->circuit = circuits->length;
        uint_buffer_push(circuits, 2);
    }
    else if (a->circuit == 0 && b->circuit != 0)
    {
        // [a] is not connected, but [b] is
        a->circuit = b->circuit;
        circuits->data[b->circuit] += 1;
    }
    else if (a->circuit != 0 && b->circuit == 0)
    {
        // [b] is not connected, but [a] is
        b->circuit = a->circuit;
        circuits->data[a->circuit] += 1;
    }
    else if (a->circuit != b->circuit)
    {
        // [a] and [b] are connected to different circuits
        // copy all boxes in [a]'s circuit to [b]'s
        // NOTE: [a]'s circuit is left in [circuits] but no boxes point to it
        int circuit = a->circuit;
        int count = 0;
        for (uint32_t idx = 0; idx < boxes->length; idx++)
        {
            if (boxes->data[idx].circuit == circuit)
            {
                boxes->data[idx].circuit = b->circuit;
                count++;
            }
        }
        circuits->data[b->circuit] += count;
    }
}

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t data_len = lseek(fd, 0, SEEK_END);
    if (data_len == -1) die("could not get file length:");
    else if (data_len == 0) die("file '%s' cannot be empty", filename);

    const char* data = mmap(0, data_len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) die("could not perform mmap:");

    size_t num_boxes = 0;
    for (const char* p = data; *p != '\0'; p++)
        if (*p == '\n')
            num_boxes++;

    box_buffer_t boxes = box_buffer();
    box_buffer_alloc(&boxes, num_boxes);

    string_t str = string_of(data_len, data);
    while (str.len > 0)
    {
        string_t line = string_split(&str, '\n');
        string_t num = string_split(&line, ',');
        box_t box;
        box.x = parse_long(num.data);
        num   = string_split(&line, ',');
        box.y = parse_long(num.data);
        num   = string_split(&line, ',');
        box.z = parse_long(num.data);
        box.circuit = 0;

        if (line.len != 0)
            die("boxes should only have 3 coordinates");

        box_buffer_push(&boxes, box);
    }

    pairs = pair_buffer();
    pair_buffer_alloc(&pairs, (num_boxes * (num_boxes - 1)) / 2);
    binary_heap_t pair_heap = binary_heap(&smaller_pair);

    for (uint32_t a = 0; a < boxes.length; a++)
    {
        for (uint32_t b = a + 1; b < boxes.length; b++)
        {
            uintptr_t idx = pairs.length;
            pair_t p = { a, b, straight_line_distance(boxes.data[a], boxes.data[b]) };
            pair_buffer_push(&pairs, p);
            binary_heap_add(&pair_heap, (void*)idx);
        }
    }

    // stores the amount of boxes in circuits that contain at least two boxes
    uint_buffer_t circuits = uint_buffer();
    void* idx;
    //                  vvvv~ annoying
    for (int i = 0; i < 1000 && binary_heap_pop(&pair_heap, &idx); i++)
    {
        pair_t* pair = &pairs.data[(uintptr_t)idx];
        box_t* a = &boxes.data[pair->box_a];
        box_t* b = &boxes.data[pair->box_b];
        connect(a, b, &circuits, &boxes);
    }

    // there may exist a better way to do this,
    // find the 3 largest circuit sizes that boxes still point to
    binary_heap_t circuit_heap = binary_heap(&bigger_integer);
    for (uint32_t i = 0; i < boxes.length; i++)
    {
        box_t b = boxes.data[i];
        if (b.circuit == 0) continue; // skip boxes that are not connected to other boxes

        int circuit_size = circuits.data[b.circuit];
        if (!binary_heap_contains(&circuit_heap, (void*)(intptr_t)circuit_size))
        {
            binary_heap_add(&circuit_heap, (void*)(intptr_t)circuit_size);
        }
    }

    long p1 = 1;
    for (uint32_t i = 0; i < 3; i++)
    {
        void* count;
        if (!binary_heap_pop(&circuit_heap, &count))
            die("less than 3 circuits exist for part 1");
        p1 *= (intptr_t)count;
    }

    printf("Part One: %ld\n", p1);

    while (binary_heap_pop(&pair_heap, &idx))
    {
        pair_t* pair = &pairs.data[(uintptr_t)idx];
        box_t* a = &boxes.data[pair->box_a];
        box_t* b = &boxes.data[pair->box_b];
        connect(a, b, &circuits, &boxes);

        if (circuits.data[a->circuit] == boxes.length)
        {
            printf("Part Two: %ld\n", (long)a->x * (long)b->x);
            goto cleanup;
        }
    }
    die("boxes do not make a single large circuit");

cleanup:
    binary_heap_free(&circuit_heap);
    uint_buffer_free(&circuits);
    binary_heap_free(&pair_heap);
    pair_buffer_free(&pairs);
    box_buffer_free(&boxes);
    if (munmap((void*)data, data_len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}
