#include <fcntl.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers.h"

DECLARE_BUFFER(point, point_t)
DEFINE_BUFFER(point, point_t)

const char* filename = "input.txt";

point_t adjacent[] = {
    { -1, -1 }, { -1, 0 }, { -1, 1 },
    {  0, -1 }, {  0, 1 },
    {  1, -1 }, {  1, 0 }, {  1, 1 },
};

static inline
char* char_at(point_t point, char* data, int line)
{
    return &data[(point.y * line) + point.x];
}

static inline
bool valid(point_t p, point_t max)
{
    return p.x >= 0 && p.x <= max.x
        && p.y >= 0 && p.y <= max.y;
}

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t len = lseek(fd, 0, SEEK_END);
    if (len == -1) die("could not get file length:");

    char* data = mmap(0, len, PROT_WRITE, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) die("could not perform mmap:");

    char* end_of_line = strpbrk(data, "\n"); // '\n' or '\0'
    if (end_of_line == NULL) die("invalid data");

    const int max_x = end_of_line - data,
              max_y = max_x, // assumed
              line_len = max_x + 1,
              num_adjacent = sizeof(adjacent) / sizeof(adjacent[0]);
    const point_t max = { max_x, max_y };
    point_buffer_t buf = point_buffer();

    int p1 = 0;
    int p2 = 0;
    for (int iter = 1;; iter++)
    {
        for (point_t cur = {0, 0};; cur.x++)
        {
            if (cur.x >= max_x)
            {
                cur.y++;
                cur.x = 0;
            }
            if (cur.y >= max_y) break;
            if (*char_at(cur, data, line_len) == '.') continue;

            int num_rolls = 0;
            for (int i = 0; i < num_adjacent; i++)
            {
                point_t point = add_point(cur, adjacent[i]);
                if (valid(point, max) && *char_at(point, data, line_len) == '@')
                    num_rolls++;
            }

            if (num_rolls < 4)
                point_buffer_push(&buf, cur);
        }

        if (iter == 1) p1 = buf.length;
        p2 += buf.length;

        if (buf.length == 0) break;
        for (uint32_t i = 0; i < buf.length; i++)
            *char_at(buf.data[i], data, line_len) = '.';
        buf.length = 0;
    }

    printf("Part One: %d\n", p1);
    printf("Part Two: %d\n", p2);

    point_buffer_free(&buf);
    if (munmap((void*)data, len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}
