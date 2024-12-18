#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "myString.c"

void panic(char *msg);
char *strdup(const char *src);
char **split_into_array(char *str, char *sep, int *size);
char *readFileToString(char *filename);

int main() {
    char *file_contents = readFileToString("test.txt");
    String contents = from_str(file_contents);
    String line_sep = from_str("\n");
    int num_lines = 0;
    String **lines = split(&contents, &line_sep, &num_lines);

    print_splits(lines, num_lines);

    free(file_contents);
    free_string(&contents);
    free_string(&line_sep);
    free_splits(lines, num_lines);
    return 0;
}

void panic(char *msg) {
    printf("%s\n", msg);
    exit(-1);
}

char *readFileToString(char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error: file %s not found", filename);
        exit(-1);
    }

    int file_length = 0;
    char *file_contents;
    char ch;

    while (fscanf(file, "%c", &ch) != EOF)
        file_length++;

    rewind(file);
    file_contents = malloc(file_length * sizeof(char));
    if (file_contents == NULL) {
        panic("no memory for file allocation");
    }

    int i = 0;
    while (fscanf(file, "%c", &ch) != EOF) {
        file_contents[i] = ch;
        i++;
    }
    file_contents[i] = '\0';
    fclose(file);

    return file_contents;
}
