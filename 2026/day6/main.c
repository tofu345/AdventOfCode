#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers/integer.h"

const char* filename = "input.txt";

static long sum(int_buffer_t buf)
{
    long res = 0;
    for (uint32_t i = 0; i < buf.length; i++)
        res += buf.data[i];
    return res;
}

static long product(int_buffer_t buf)
{
    long res = 1;
    for (uint32_t i = 0; i < buf.length; i++)
        res *= buf.data[i];
    return res;
}

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t data_len = lseek(fd, 0, SEEK_END);
    if (data_len == -1) die("could not get file length:");

    const char* data = mmap(0, data_len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) die("could not perform mmap:");

    if (data_len <= 2) die("data cannot possibly be less than/equal to 2 characters");

    const char* signs;
    for (int i = data_len - 2; i > 0; i--)
    {
        if (data[i] == '\n')
        {
            signs = &data[i + 1];
            break;
        }
    }

    if (signs[0] != '+' && signs[0] != '*') die("last line of data must start with '*' or '+'");

    const char* line_end = strchr(data, '\n');
    if (line_end == NULL) die("data must contain an least one line");
    int line_length = line_end - data + 1;
    int num_lines = data_len / line_length;

    int cur_sign_idx = 0;
    long p1 = 0;

    // certainly not necessary but..
    int_buffer_t nums = int_buffer();

loop:
    for (int line = 0; line < num_lines - 1; line++)
    {
        long num = parse_long(&data[(line_length * line) + cur_sign_idx]);
        int_buffer_push(&nums, num);
    }

    if (signs[cur_sign_idx] == '+')
        p1 += sum(nums);
    else if (signs[cur_sign_idx] == '*')
        p1 += product(nums);
    nums.length = 0;

    cur_sign_idx++;
    for (; cur_sign_idx < line_length; cur_sign_idx++)
        if (signs[cur_sign_idx] == '+' || signs[cur_sign_idx] == '*')
            goto loop;

    printf("Part One: %ld\n", p1);

    // inspired by https://www.reddit.com/r/adventofcode/comments/1pfhg5t/2025_day_6_part_2_visualization_for_the_sample/#lightbox
    // its beautiful
    cur_sign_idx = 0;
    long p2 = 0;
    int offset = 0;
    while (1)
    {
        long num = 0;
        bool empty_column = true;
        for (int line = 0; line < num_lines - 1; line++)
        {
            char ch = data[(line_length * line) + cur_sign_idx + offset];
            if (ch == '\n' || ch == '\0')
                break;

            if (ch != ' ')
            {
                num *= 10;
                num += ch - 48;
                empty_column = false;
            }
        }

        if (empty_column)
        {
            if (signs[cur_sign_idx] == '+')
                p2 += sum(nums);
            else if (signs[cur_sign_idx] == '*')
                p2 += product(nums);
            nums.length = 0;

            cur_sign_idx++;
            for (; cur_sign_idx < line_length
                    && signs[cur_sign_idx] != '+'
                    && signs[cur_sign_idx] != '*'; cur_sign_idx++);

            if (cur_sign_idx >= line_length)
                goto end;

            offset = 0;
        }
        else
        {
            int_buffer_push(&nums, num);
            offset++;
        }
    }
end:
    printf("Part Two: %ld\n", p2);

    int_buffer_free(&nums);
    if (munmap((void*)data, data_len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}
