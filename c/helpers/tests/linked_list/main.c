#include "../unity/unity.h"

#include "helpers/linked_list.h"

#include <stdbool.h>

#define LENGTH(x) (sizeof(x) / sizeof(x[0]))

arena_t arena;
linked_list_t list;
linked_list_t * l = &list;

void setUp()
{
    arena = arena_new();
    list = linked_list_new(&arena);
}

void tearDown()
{
    arena_free(&arena);
}

static bool linked_list_is(linked_list_t * list, int data[], int len)
{
    node_t * cur = list->head;
    int i = 0;
    for (; i < len && cur != NULL; i++, cur = cur->next)
    {
        if ((intptr_t)cur->data != (intptr_t)data[i])
            return false;
    }
    return cur == NULL;
}

static void test_insert_find(void)
{
    {
        linked_list_prepend(l, (void *)1);
        linked_list_prepend(l, (void *)2);
        linked_list_prepend(l, (void *)3);
        linked_list_prepend(l, (void *)4);
        linked_list_prepend(l, (void *)5);

        int data[] = { 5, 4, 3, 2, 1 };
        TEST_ASSERT(linked_list_is(l, data, LENGTH(data)));
    }

    {
        node_t * n = linked_list_find(l, (void *)3);
        TEST_ASSERT_MESSAGE(n != NULL, "could not find node with data 3");

        linked_list_append(l, n, (void *)-1);
        linked_list_append(l, n, (void *)-2);
        linked_list_append(l, n, (void *)-3);

        int data[] = { 5, 4, 3, -3, -2, -1, 2, 1 };
        TEST_ASSERT(linked_list_is(l, data, LENGTH(data)));
    }

    {
        linked_list_append(l, l->head, (void *)6);
        linked_list_append(l, l->head, (void *)7);

        int data[] = { 5, 7, 6, 4, 3, -3, -2, -1, 2, 1 };
        TEST_ASSERT(linked_list_is(l, data, LENGTH(data)));
    }

    {
        node_t * n = linked_list_find(l, (void *)1);
        TEST_ASSERT_MESSAGE(n != NULL, "could not find node with data 1");

        linked_list_append(l, n, (void *)10);
        linked_list_append(l, n, (void *)11);

        int data[] = { 5, 7, 6, 4, 3, -3, -2, -1, 2, 1, 11, 10 };
        TEST_ASSERT(linked_list_is(l, data, LENGTH(data)));
    }
}

int main(void)
{
    UNITY_BEGIN();
    RUN_TEST(test_insert_find);
    return UNITY_END();
}
