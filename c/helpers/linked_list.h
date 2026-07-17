#ifndef HELPERS_LINKED_LIST_H
#define HELPERS_LINKED_LIST_H

#include <stddef.h>

#include "helpers/arena.h"

typedef struct node {
    void * data;
    struct node * next;
} node_t;

typedef struct {
    node_t * head;
    arena_t * arena; // for better caching of `node_t` maybe?
} linked_list_t;

static inline linked_list_t linked_list_new(arena_t * arena)
{
    return (linked_list_t){ .head = NULL, .arena = arena };
}

// prepend a new `node_t` with [data] to [list->head].
void linked_list_prepend(linked_list_t * list, void * data);

// append a new `node_t` with [data] to [n].
void linked_list_append(linked_list_t * list, node_t * n, void * data);

node_t * linked_list_find(linked_list_t * list, void * data);

node_t * linked_list_find_from(node_t * n, void * data);

#endif // HELPERS_LINKED_LIST_H
