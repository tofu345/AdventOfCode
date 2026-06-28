#include "../unity/unity.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "helpers/binary_heap.h"

#define ADD(h, n) binary_heap_add(&h, (void *)(intptr_t)n);
#define DEL(h, n) binary_heap_delete(&h, (void *)(intptr_t)n);

void setUp() {}
void tearDown() {}

static void assert_contains(binary_heap_t *heap, void *elems[], int len);

bool compare_int(void *a, void *b)
{
    return (intptr_t)a <= (intptr_t)b;
}

void display_int(void *num)
{
    printf("%ld\n", (intptr_t)num);
}

bool compare_str(void *a, void *b)
{
    const char *ap = a, *bp = b;
    uint32_t a_sum = 0, b_sum = 0;
    for (; *ap != '\0'; ap++) a_sum += (uint32_t)*ap;
    for (; *bp != '\0'; bp++) b_sum += (uint32_t)*bp;
    return a_sum <= b_sum;
}

void display_str(void *num)
{
    printf("%s\n", (char *)num);
}

void test_str(void)
{
    binary_heap_t heap = binary_heap(&compare_str);

    const char *elems[] = { "a", "b", "c", "def", "abc", "xyz", "abcdefg", "ghtaoeu" };
    int len = sizeof(elems) / sizeof(elems[0]);

    for (int i = 0; i < len; i++)
    {
        ADD(heap, elems[i]);
    }
    assert_contains(&heap, (void **)elems, len);
    TEST_ASSERT_TRUE(is_binary_heap(&heap));

    // binary_heap_display(&heap, &display_str);
    // printf("-----\n");

    const char *exp[] = { "a", "b", "c", "abc", "def", "xyz", "abcdefg", "ghtaoeu" };
    for (uint32_t i = 0; i < heap.length; i++)
    {
        void* res;
        TEST_ASSERT_TRUE(binary_heap_pop(&heap, &res));
        TEST_ASSERT_EQUAL_STRING(exp[i], res);
        TEST_ASSERT_TRUE(is_binary_heap(&heap));

        // binary_heap_display(&heap, &display_str);
        // printf("-----\n");
    }

    binary_heap_free(&heap);
}

void test_int(void)
{
    binary_heap_t heap = binary_heap(&compare_int);

    intptr_t elems[] = { 1, 7, 3, 5, 6, 7, 8, 10, 0, -1, -9, -5, -7, 15, 100, -1000 };
    int len = sizeof(elems) / sizeof(elems[0]);

    for (int i = 0; i < len; i++)
    {
        ADD(heap, elems[i]);
    }
    assert_contains(&heap, (void **)elems, len);
    TEST_ASSERT_TRUE(is_binary_heap(&heap));

    // binary_heap_display(&heap, &display_int);
    // printf("-----\n");

    intptr_t exp[] = { -1000, -9, -7, -5, -1, 0, 1, 3, 5, 6, 7, 7, 8, 10, 15, 100 };
    for (uint32_t i = 0; i < heap.length; i++)
    {
        void* res;
        TEST_ASSERT_TRUE(binary_heap_pop(&heap, &res));
        TEST_ASSERT_EQUAL(exp[i], res);
        TEST_ASSERT_TRUE(is_binary_heap(&heap));

        // binary_heap_display(&heap, &display_int);
        // printf("-----\n");
    }

    binary_heap_free(&heap);
}

static void assert_contains(binary_heap_t *heap, void *elems[], int len)
{
    for (int i = 0; i < len; i++)
        TEST_ASSERT_TRUE(binary_heap_contains(heap, (void *)(intptr_t)elems[i]));
}

int main(void)
{
    UNITY_BEGIN();
    RUN_TEST(test_int);
    RUN_TEST(test_str);
    return UNITY_END();
}
