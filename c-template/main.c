#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "helpers.h"

const char* filename = "test.txt";

int main(void)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1) die("could not open '%s':", filename);

    off_t data_len = lseek(fd, 0, SEEK_END);
    if (data_len == -1) die("could not get file length:");

    const char* data = mmap(0, data_len, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) die("could not perform mmap:");

    // printf("%s\n", data);

    if (munmap((void*)data, data_len) == -1) die("could not perform munmap:");
    if (close(fd) == -1) die("could not close fd:");
    return 0;
}
