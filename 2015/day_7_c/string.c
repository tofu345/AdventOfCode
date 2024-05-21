#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULT_STRING_CAP 8

typedef struct {
    char *str;
    int len;
    int cap;
} String;

String new_string();
String from_str(char *str);
void push(String *s, char c);
void push_str(String *s, char *str);
void free_string(String *s);
void realloc_string(String *s);

// Creates new empty string
//
// ```
// String s = new_string();
//
// // -- code
//
// free_string(&s); // Do not forget this
// ```
String new_string() {
    String s;
    s.len = 0;
    s.cap = DEFAULT_STRING_CAP;
    s.str = malloc(sizeof(char) * s.cap);
    if (s.str == NULL) {
        printf("no memory\n");
    }

    return s;
}

String from_str(char *str) {
    String s = new_string();
    push_str(&s, str);
    return s;
}

void realloc_string(String *s) {
    s->cap *= 2;
    s->str = realloc(s->str, s->cap);
}

void push(String *s, char c) {
    if ((s->len + 1) > s->cap) {
        realloc_string(s);
    }

    s->str[s->len] = c;
    s->str[s->len + 1] = '\0';
    s->len++;
}

void push_str(String *s, char *str) {
    int str_len = strlen(str);
    for (int i = 0; i < str_len; i++) {
        push(s, str[i]);
    }
}

void free_string(String *s) { free(s->str); }
