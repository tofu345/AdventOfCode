#include "../unity/unity.h"

#include "ring_buffer.h"

#define LENGTH(arr) (sizeof(arr) / sizeof(arr[0]))

ring_buffer_t buffer;
ring_buffer_t * rb = &buffer;

void setUp()
{
    buffer = ring_buffer();
}
void tearDown()
{
    ring_buffer_free(rb);
}

static bool rb_is(uintptr_t exp[], int len)
{
    uint32_t i = rb->start;
    for (int j = 0; j < len; i++, j++)
    {
        if (i == rb->capacity)
            i = 0;
        if ((uintptr_t)rb->data[i] != exp[j])
            return false;
    }
    return true;
}

static void test(void)
{
    for (uintptr_t i = 1; i <= 15; i++)
    {
        ring_buffer_push(rb, (void *)i);
    }

    {
        uintptr_t exp[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
        TEST_ASSERT(rb_is(exp, LENGTH(exp)));
    }

    ring_buffer_pop(rb);
    ring_buffer_pop(rb);
    ring_buffer_push(rb, (void *)17);

    {
        uintptr_t exp[] = { 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17 };
        TEST_ASSERT(rb_is(exp, LENGTH(exp)));
    }

    ring_buffer_push(rb, (void *)1);
    ring_buffer_pop(rb);
    ring_buffer_push(rb, (void *)2);
    ring_buffer_pop(rb);

    {
        uintptr_t exp[] = { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 1, 2 };
        TEST_ASSERT(rb_is(exp, LENGTH(exp)));
    }

    ring_buffer_push(rb, (void *)3);
    ring_buffer_push(rb, (void *)2);
    ring_buffer_push(rb, (void *)5);

    {
        uintptr_t exp[] = { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 1, 2, 3, 2, 5 };
        TEST_ASSERT(rb_is(exp, LENGTH(exp)));
    }

    // for (uint32_t i = 0; i < rb->capacity; i++)
    // {
    //     printf("%ld ", (uintptr_t)rb->data[i]);
    // }
    // printf("\n");
}

int main(void)
{
    UNITY_BEGIN();
    RUN_TEST(test);
    return UNITY_END();
}
