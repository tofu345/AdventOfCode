#include "linked_list.h"
#include "utils.h"

#include <assert.h>

void linked_list_free(linked_list_t * list)
{
    assert(list != NULL && "cannot free NULL linked list");

    node_t * cur = list->head;
    while (cur != NULL)
    {
        node_t * next = cur->next;
        free(cur);
        cur = next;
    }
    list->head = NULL;
}

void linked_list_prepend(linked_list_t * list, void * data)
{
    assert(list != NULL && "cannot prepend to NULL linked list");

    node_t * new = arena_alloc(list->arena, sizeof(node_t));
    if (new == NULL) die("linked_list_prepend:");

    new->data = data;
    new->next = list->head;
    list->head = new;
}

void linked_list_append(linked_list_t * list, node_t * n, void * data)
{
    assert(n != NULL && "cannot append to NULL node");

    node_t * new = arena_alloc(list->arena, sizeof(node_t));
    if (n == NULL) die("linked_list_append:");

    new->data = data;
    new->next = n->next;
    n->next = new;
}

node_t * linked_list_find(linked_list_t * list, void * data)
{
    assert(list != NULL && "cannot search NULL linked list");

    node_t * cur = list->head;
    while (cur != NULL)
    {
        if (cur->data == data) return cur;
        cur = cur->next;
    }
    return NULL;
}

node_t * linked_list_find_from(node_t * n, void * data)
{
    assert(n != NULL && "cannot search from NULL node");

    while (n != NULL)
    {
        if (n->data == data) return n;
        n = n->next;
    }
    return NULL;
}
