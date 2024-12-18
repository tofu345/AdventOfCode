#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array_map.c"

void free_splits(char **splits, int len);
void panic(char *msg);
char *strdup(const char *src);
char **split_into_array(char *str, char *sep, int *size);
char *read_to_string(char *filename);
int seperator_count(char *str, char *sep);

int main() {
    char *file_contents = read_to_string("test.txt");
    ArrayMap map = new_arr();

    char sep[] = "\n";
    int splits_len;
    char **splits = split_into_array(file_contents, sep, &splits_len);

    for (int i = 0; i < splits_len; i++) {
        char *line = splits[i];
        // printf("%s\n", line);

        char *sep = " -> ";
        int split2_len;
        char **splits2 = split_into_array(line, sep, &split2_len);
        if (splits2 == NULL) {
            // printf("no splits for %s\n", line);
            continue;
        }

        for (int j = 0; j < split2_len; j++) {
            printf("-> %s\n", splits2[j]);
        }

        free_splits(splits2, split2_len);
    }

    free_splits(splits, splits_len);
    free(file_contents);
    free_arr(&map);
    return 0;
}

void panic(char *msg) {
    printf("%s\n", msg);
    exit(-1);
}

void free_splits(char **splits, int len) {
    for (int i = 0; i < len; i++) {
        free(splits[i]);
    }
    free(splits);
}

char **split_into_array(char *str, char *sep, int *size) {
    int num_sep = seperator_count(str, sep);
    if (num_sep == -1) {
        return NULL;
    }

    *size = num_sep + 1;
    char **splits = malloc(num_sep * sizeof(char *));
    if (splits == NULL) {
        panic("no memory");
    }

    int str_len = strlen(str);
    int sep_len = strlen(sep);
    int split_index = 0;
    int k = 0;
    char split[500];
    for (int i = 0; i < str_len; i++) {
        if (str[i] == sep[0]) {
            int match = 1;
            for (int j = 1; j < sep_len; j++) {
                if (sep[j] != str[i + j]) {
                    match = 0;
                    break;
                }
            }

            if (match == 1) {
                split[k] = '\0';
                splits[split_index] = strdup(split);
                split_index++;
                i += sep_len;
                strncpy(split, "", 3);
                k = 0;
            }
        }

        split[k] = str[i];
        k++;
    }

    split[k] = '\0';
    splits[split_index] = strdup(split);

    return splits;
}

// Duplicate string
//
// src: https://stackoverflow.com/questions/252782/strdup-what-does-it-do-in-c/252802#252802
char *strdup(const char *src) {
    char *dst = malloc(strlen(src) + 1); // Space for length plus nul
    if (dst == NULL) {
        // No memory
        printf("no memory\n");
        exit(-1);
    }
    strcpy(dst, src); // Copy the characters
    return dst;       // Return the new string
}

int seperator_count(char *str, char *sep) {
    int str_len = strlen(str);
    int sep_len = strlen(sep);

    if (sep_len > str_len) {
        return -1;
    }

    int sep_count = 0;
    for (int i = 0; i < str_len; i++) {
        if (str[i] == sep[0]) {
            int match = 1;
            for (int j = 1; j < sep_len; j++) {
                if (sep[j] != str[i + j]) {
                    match = 0;
                    break;
                }
            }

            if (match == 1)
                sep_count++;
        }
    }

    return sep_count;
}

char *read_to_string(char *filename) {
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
