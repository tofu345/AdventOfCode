#include <assert.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers.h"
#include "helpers/string_slice.h"
#include "helpers/integer.h"
#include "helpers/binary_heap.h"
#include "helpers/hash_table.h"
#include "helpers/arena.h"
#include "helpers/linked_list.h"

// looks nicer in lower case
#define voidp_from_int(x) ((void *)(uintptr_t)x)
#define int_from_voidp(x) ((int)(uintptr_t)x)

const char * filename = "input.txt";

typedef struct {
    int x, y;
} vertex_t;

typedef struct {
    //  start, end
    int x1, y1,
        x2, y2;
    long area;
} rect_t;

// a line of green/red tiles that has been previously checked
typedef struct {
    int start, end;
} seen_line_t;

// this is getting out of hand.
DECLARE_BUFFER(vertex, vertex_t) // not necessary
DEFINE_BUFFER(vertex, vertex_t)
DECLARE_BUFFER(rect, rect_t)
DEFINE_BUFFER(rect, rect_t)
DECLARE_BUFFER(seen, seen_line_t)
DEFINE_BUFFER(seen, seen_line_t)

bool larger_rect(void * a, void * b)
{
    return ((rect_t *)a)->area >= ((rect_t *)b)->area;
}

int sort_nums(const void * a, const void * b)
{
    return *(const int *)a - *(const int *)b;
}

// https://www.geeksforgeeks.org/dsa/how-to-check-if-a-given-point-lies-inside-a-polygon/
static inline bool on_edge(uint64_t x1, uint64_t y1, uint64_t x2, uint64_t y2,
                           uint64_t x, uint64_t y)
{
    uint64_t c = (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1);
    if (c != 0) return false;

    return MIN(x1, x2) <= x && x <= MAX(x1, x2)
        && MIN(y1, y2) <= y && y <= MAX(y1, y2);
}
bool is_inside(vertex_t vert[], int nvert, uint64_t x, uint64_t y, int * edges)
{
    assert(edges != NULL);

    bool inside = false;

    // the number of edges right of (x, y)
    *edges = 0;
    for (int i = 0, j = nvert - 1; i < nvert; j = i++)
    {
        uint64_t x1 = vert[i].x, y1 = vert[i].y,
                 x2 = vert[j].x, y2 = vert[j].y;

        if (on_edge(x1, y1, x2, y2, x, y)) return true;

        bool intersect = ((y1 > y) != (y2 > y))
                       && (x < (double)(x2 - x1) * (y - y1) / (y2 - y1) + x1);

        if (intersect)
        {
            *edges += 1;
            inside = !inside;
        }
    }
    return inside;
}

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t data_len = lseek(fd, 0, SEEK_END);
    if (data_len == -1) die("could not get file length:");
    else if (data_len == 0) die("file '%s' cannot be empty", filename);

    const char * data = mmap(0, data_len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) die("could not perform mmap:");

    vertex_buffer_t vertices = vertex_buffer();
    string_t data_string = string_of(data_len, data);
    while (data_string.len > 0)
    {
        string_t line = string_split(&data_string, '\n'),
                 num  = string_split(&line, ',');
        vertex_t t;
        t.x = parse_long(num.data);
        num = string_split(&line, ',');
        t.y = parse_long(num.data);
        vertex_buffer_push(&vertices, t);
    }

    if (vertices.length == 0) die("no vertices provided");

    // compress coordinates
    // https://www.youtube.com/watch?v=s9oXy-fZUzg
    int_buffer_t compressed = int_buffer();
    compressed.length = vertices.length * 2;
    int_buffer_alloc(&compressed, compressed.length);
    for (uint32_t i = 0, j = 0; i < compressed.length; i += 2, j++)
    {
        compressed.data[i] = vertices.data[j].x;
        compressed.data[i + 1] = vertices.data[j].y;
    }

    qsort(compressed.data, compressed.length, sizeof(int), sort_nums);
    hash_table_t compressed_coords = hash_table();

    int prev = compressed.data[0];
    compressed.data[0] = 1;
    for (uint32_t i = 1; i < compressed.length; i++)
    {
        int uncompressed = compressed.data[i];
        if (compressed.data[i] == prev)
        {
            compressed.data[i] = compressed.data[i-1];
            hash_table_set(&compressed_coords, hash_bits(uncompressed, 0),
                           voidp_from_int(uncompressed),
                           voidp_from_int(compressed.data[i]));
        }
        else
            compressed.data[i] = compressed.data[i-1] + 1;
        prev = uncompressed;
    }
    int_buffer_free(&compressed);

    // stores all combinations of 2 vertices
    rect_buffer_t rects = rect_buffer();
    uint32_t num_rects = (vertices.length * (vertices.length - 1)) / 2;
    // preallocate to avoid having realloc invalidate pointers in the binary heap.
    rect_buffer_alloc(&rects, num_rects);
    // stores rectangles, ordered from largest to smallest
    binary_heap_t rect_heap = binary_heap_of(num_rects, &larger_rect);

    for (uint32_t i = 0; i < vertices.length; i++)
    {
        for (uint32_t j = i + 1; j < vertices.length; j++)
        {
            rect_t rect;
            rect.x1 = MIN(vertices.data[i].x, vertices.data[j].x);
            rect.x2 = MAX(vertices.data[i].x, vertices.data[j].x);
            rect.y1 = MIN(vertices.data[i].y, vertices.data[j].y);
            rect.y2 = MAX(vertices.data[i].y, vertices.data[j].y);
            rect.area = ((long)(rect.x2 - rect.x1) + 1) * ((long)(rect.y2 - rect.y1) + 1);

            // compress rect coordinates
            void * res;
            hash_table_get(&compressed_coords, hash_bits(rect.x1, 0), &res);
            rect.x1 = int_from_voidp(res);
            hash_table_get(&compressed_coords, hash_bits(rect.x2, 0), &res);
            rect.x2 = int_from_voidp(res);
            hash_table_get(&compressed_coords, hash_bits(rect.y1, 0), &res);
            rect.y1 = int_from_voidp(res);
            hash_table_get(&compressed_coords, hash_bits(rect.y2, 0), &res);
            rect.y2 = int_from_voidp(res);

            int pos = rects.length;
            rect_buffer_push(&rects, rect);
            binary_heap_add(&rect_heap, &rects.data[pos]);
        }
    }

    // convert `vertices` to compressed coordinates
    for (uint32_t i = 0; i < vertices.length; i++)
    {
        vertex_t * v = &vertices.data[i];
        void * res;

        hash_table_get(&compressed_coords, hash_bits(v->x, 0), &res);
        v->x = (int)(uintptr_t)res;
        hash_table_get(&compressed_coords, hash_bits(v->y, 0), &res);
        v->y = (int)(uintptr_t)res;
    }

    void * res;
    if (binary_heap_peek(&rect_heap, &res) == false)
        die("no rectangles were formed");

    printf("Part One: %ld\n", ((rect_t *)res)->area);

    arena_t arena = arena_new();
    hash_table_t ht = hash_table();
    while (binary_heap_pop(&rect_heap, &res))
    {
        rect_t * r = res;

        // check if all lines in the rect are in the loop
        for (int y = r->y1; y <= r->y2; y++)
        {
            // where previously seen lines are stored
            seen_buffer_t * seen = NULL;
            uint32_t hash = hash_bits(y, 0);

            // grouped by [y] value
            hash_table_get(&ht, hash, (void **)&seen);

            // check if the current line has been seen before
            if (seen != NULL)
                // search in reverse, from most recent
                for (int i = seen->length - 1; i >= 0; i--)
                    if (r->x1 >= seen->data[i].start && r->x2 <= seen->data[i].end)
                    {
                        // make most recent
                        seen_line_t last = seen->data[seen->length - 1];
                        seen->data[seen->length - 1] = seen->data[i];
                        seen->data[i] = last;
                        goto next_line;
                    }

            // uses raytracing
            // a line does not contain any holes if there are no edges between [r.x1] and [r.x2]
            // which is the difference between the edges right of [r.x2] and [r.x1]
            int edges1, edges2;
            if (r->x1 >= r->x2
                || !is_inside(vertices.data, vertices.length, r->x1, y, &edges1)
                || !is_inside(vertices.data, vertices.length, r->x2, y, &edges2)
                || edges2 - edges1 > 0)
                goto next_rect;

            if (seen == NULL)
            {
                seen = arena_alloc(&arena, sizeof(seen_buffer_t));
                *seen = seen_buffer();
                hash_table_set(&ht, hash, seen, seen);
            }

            // mark the current line as seen
            seen_buffer_push(seen, (seen_line_t){ .start = r->x1, .end = r->x2 });
next_line:
        }

        printf("Part Two: %ld\n", r->area);
        goto cleanup;
next_rect:
    }

cleanup:
    hash_table_free(&compressed_coords);
    hash_table_iter_t iter = hash_table_iter(&ht);
    hash_table_entry_t * entry;
    while ((entry = hash_table_iter_next(&iter)))
        seen_buffer_free(entry->key);
    arena_free(&arena);
    hash_table_free(&ht);
    binary_heap_free(&rect_heap);
    rect_buffer_free(&rects);
    vertex_buffer_free(&vertices);
    if (munmap((void *)data, data_len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}
