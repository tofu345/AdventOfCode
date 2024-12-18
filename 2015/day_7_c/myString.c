#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULT_STRING_CAP 8

// Dynamically expanding String
typedef struct {
    char *str;
    int len;
    int cap;
} String;

// create new empty string
//
// String s = new_string();
String new_string();
// copies char * into String
String from_str(char *str);
// Append char to String
void push(String *s, char c);
// Append char * to String
void push_str(String *s, char *str);
// free String
void free_string(String *s);
// sets String capacity
void set_string_cap(String *s, int new_cap);
// print containing String
void print_string(String *s);
void print_splits(String **s, int len);
// returns 1 if strings are the same else 0
int cmp_string(String *s1, String *s2);
// number of times sep occurs in str
//
// returns -1 if sep is larger than str
int count_sep(String *str, String *sep);
// returns 1 if s2 from s2_idx is substring of s1 from s1_idx else 0
int cmp_substr(String *s1, int s1_idx, String *s2, int s2_idx);
// spits str on sep
String** split(String *str, String *sep, int *size);
// free splits
void free_splits(String **s, int len);

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

// Duplicate string
//
// src:
// https://stackoverflow.com/questions/252782/strdup-what-does-it-do-in-c/252802#252802
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

String from_str(char *str) {
    String s = new_string();
    s.str = strdup(str);
    s.len = strlen(str);
    return s;
}

void set_string_cap(String *s, int new_cap) {
    if (new_cap < s->cap) {
        printf("Err: new String cap less than current");
        return;
    }

    s->cap = new_cap;
    s->str = realloc(s->str, s->cap);
}

void push(String *s, char c) {
    if ((s->len + 1) > s->cap) {
        set_string_cap(s, s->cap * 2);
    }

    s->str[s->len] = c;
    s->str[s->len + 1] = '\0';
    s->len++;
}

void push_str(String *s, char *str) {
    int str_len = strlen(str);
    if (s->len + str_len + 1 > s->cap) {
        set_string_cap(s, s->cap * 2);
        push_str(s, str);
        return;
    }

    for (int i = 0; i < str_len; i++) {
        s->str[s->len + i] = str[i];
    }
    s->len = s->len + str_len;
    s->str[s->len] = '\0';
}

void free_string(String *s) { free(s->str); }

void print_string(String *s) { printf("%s", s->str); }

void print_splits(String **s, int len) {
    for (int i = 0; i < len; i++) {
        print_string(s[i]);
        printf("\n");
    }
}

int cmp_string(String *s1, String *s2) {
    if (s1->len != s2->len) {
        return 0;
    }
    for (int i = 0; i < s1->len; i++) {
        if (s1->str[i] != s2->str[i]) {
            return 0;
        }
    }
    return 1;
}

int cmp_substr(String *s1, int s1_idx, String *s2, int s2_idx) {
    if (s1->len - s1_idx < s2->len - s2_idx) {
        return 0;
    }
    for (int i = 0; i < s2->len; i++) {
        if (s2->str[s2_idx + i] != s1->str[s1_idx + i]) {
            return 0;
        }
    }
    return 1;
}

int count_sep(String *s, String *sep) {
    if (sep->len > s->len) {
        return -1;
    }

    int sep_count = 0;
    int skip = 0;
    for (int i = 0; i < s->len; i++) {
        if (skip > 0) {
            skip--;
            continue;
        }

        if (s->str[i] == sep->str[0]) {
            if (cmp_substr(s, i, sep, 0)) {
                sep_count++;
                skip = sep->len;
            }
        }
    }

    return sep_count;
}

String** split(String *str, String *sep, int *size) {
    int seps = count_sep(str, sep);
    if (seps <= 0) {
        return NULL;
    }

    String **splits = malloc(seps * sizeof(String *));
    if (splits == NULL) {
        return NULL;
    }
   
    int idx = 0;
    int last_split = 0;
    for (int i = 0; i < str->len && idx < seps; i++) {
        if (cmp_substr(str, i, sep, 0)) {
            splits[idx] = malloc(sizeof(String));
            splits[idx]->len = 0;
            splits[idx]->cap = DEFAULT_STRING_CAP;
            splits[idx]->str = malloc(sizeof(char) * splits[idx]->cap);
            for (int j = last_split; j < i; j++) {
                push(splits[idx], str->str[j]);
            }
            last_split = i + sep->len;
            if (splits[idx]->str == NULL) {
                printf("no memory\n");
                exit(-1);
            }
            idx++;
        }
    }

    if (last_split != str->len) {
        splits[seps] = malloc(sizeof(String));
        splits[seps]->len = 0;
        splits[seps]->cap = DEFAULT_STRING_CAP;
        splits[seps]->str = malloc(sizeof(char) * splits[idx]->cap);
        for (int j = last_split + 1; j < str->len; j++) {
            push(splits[seps], str->str[j]);
        }
        if (splits[seps]->str == NULL) {
            printf("no memory\n");
            exit(-1);
        }
        seps++;
    }

    *size = seps;
    return splits;
}

void free_splits(String **s, int len) { 
    for (int i = 0; i < len; i++) {
        free(s[i]->str);
        free(s[i]);
    }
    free(s);
}
