#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *read_to_string(char *filename);
void panic(char *msg);

int main(int argc, char *argv[]) {
    printf("hello\n");

    char *filename = "input.txt";
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error: file %s not found", filename);
        exit(-1);
    }

    int i = 0;
    int floor = 0;
    char ch;
    while (fscanf(file, "%c", &ch) != EOF) {
        if (ch == '\n') {
            continue;
        }

        if (ch == '(') {
            floor++;
        } else if (ch == ')') {
            floor--;
        }

        if (floor == -1) {
            printf("Santa enters the basement at position %d\n", i + 1);
            break;
        }
        i++;
    }
    fclose(file);
    return 0;
}

void panic(char *msg) {
    printf("%s\n", msg);
    exit(-1);
}
