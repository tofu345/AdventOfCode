#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers.h"
#include "helpers/integer.h"

// copied this person for part2, the idea is brilliant
// https://www.reddit.com/r/adventofcode/comments/1pg9w66/comment/nveor3r/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

char* filename = "input.txt";

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t data_len = lseek(fd, 0, SEEK_END);
    if (data_len == -1) die("could not get file length:");

    char* data = mmap(0, data_len, PROT_WRITE, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) die("could not perform mmap:");

    char* start = strchr(data, 'S');
    if (start == NULL) die("cannot find tachyon entry point");
    *start = '|';

    char* line = strchr(start, '\n');
    if (line == NULL) die("only one line");
    int line_length = line - data;
    line++;

    long_buffer_t buf = {0};
    long_buffer_fill(&buf, 0, line_length);
    buf.data[start - data] = 1;

    int p1 = 0;
    while (1)
    {
        int i = 0;
        for (; line[i] != '\0' && line[i] != '\n'; i++)
        {
            if (line[i] == '^' && data[i] == '|')
            {
                p1++;
                data[i] = '.';

                data[i - 1] = '|';
                buf.data[i - 1] += buf.data[i];

                data[i + 1] = '|';
                buf.data[i + 1] += buf.data[i];

                buf.data[i] = 0;
            }
        }

        if (line[i] == '\0') break;
        line += i + 1;
    }

    printf("Part One: %d\n", p1);

    long p2 = 0;
    for (uint32_t i = 0; i < buf.length; i++)
        p2 += buf.data[i];

    printf("Part Two: %ld\n", p2);

    long_buffer_free(&buf);
    if (munmap((void*)data, data_len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}
