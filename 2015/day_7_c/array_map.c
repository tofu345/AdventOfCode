#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INIT_CAPACITY 4
#define MAX_STR_LENGTH 100

typedef struct {
    char key[MAX_STR_LENGTH];
    char val[MAX_STR_LENGTH];
} Item;

typedef struct {
    Item *items;
    int len;
    int cap;
} ArrayMap;

ArrayMap new_arr();
int contains(ArrayMap *v, char *key);
void free_arr(ArrayMap *v);
int insert(ArrayMap *v, char *key, char *val);

// Creates new empty array map
//
// An array map is a dynamic array of key value pairs
// but it does not allow duplicate keys like a hashmap
//
// ```
// ArrayMap v = new_vector();
//
// // -- code
//
// free_vec(&v); // Do not forget this
// ```
ArrayMap new_arr() {
    ArrayMap v;
    v.items = malloc(INIT_CAPACITY * sizeof(Item));
    if (v.items == NULL) {
        printf("no memory for array map allocation\n");
        exit(-1);
    }

    v.cap = INIT_CAPACITY;
    v.len = 0;
    return v;
}

void free_arr(ArrayMap *v) { free(v->items); }

// Inserts into array map key value pair, replacing duplicate keys
// return 1 if replaced 0 if not
int insert(ArrayMap *v, char *key, char *val) {
    if (strlen(key) > MAX_STR_LENGTH) {
        printf("key size exceeds maximum, truncating\n");
    }

    if (strlen(val) > MAX_STR_LENGTH) {
        printf("value size exceeds maximum, truncating\n");
    }

    if ((v->len + 1) > v->cap) {
        // printf("reallocating\n");
        int new_capacity = v->cap * 2;
        v->items = realloc(v->items, new_capacity * sizeof(Item));
        v->cap = new_capacity;
    }

    int duplicate_index = contains(v, key);
    if (duplicate_index == -1) {
        strcpy(v->items[v->len].key, key);
        strcpy(v->items[v->len].val, val);
        v->len++;
        return 0;
    }

    printf("duplicate key, replacing\n");
    Item *new_items = malloc((v->cap) * sizeof(Item));
    if (new_items == NULL) {
        printf("no memory for array map allocation\n");
        exit(-1);
    }

    int j = 0;
    for (int i = 0; i < v->len; i++) {
        if (i != duplicate_index) {
            strcpy(new_items[j].key, v->items[i].key);
            strcpy(new_items[j].val, v->items[i].val);
            j++;
        }
    }

    strcpy(new_items[j].key, key);
    strcpy(new_items[j].val, val);

    free(v->items);
    v->items = new_items;
    return 1;
}

// returns -1 if array map does not contain key else index of key
int contains(ArrayMap *v, char *key) {
    for (int i = 0; i < v->len; i++) {
        if (strcmp(v->items[i].key, key) == 0) {
            // printf("duplicate key %s\n", key);
            return i;
        }
    }

    return -1;
}
