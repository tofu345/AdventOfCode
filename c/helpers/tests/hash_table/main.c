#include "hash_table.h"

#include "../unity/unity.h"

#include <stdio.h>

#define LENGTH(x) (sizeof(x) / sizeof(x[0]))

hash_table_t ht;

void setUp()
{
    ht = hash_table();
}

void tearDown()
{
    hash_table_free(&ht);
}

static inline bool get(uint64_t hash, void* expected_val)
{
    void* val;
    if (hash_table_get(&ht, hash, &val) == false) return false;
    return expected_val == val;
}

static inline bool set(uint64_t hash, void* key, void* val)
{
    return hash_table_set(&ht, hash, key, val) == true;
}

// *

static void assert_get_str(const char* key, const char* expected_val)
{
    if (!get(hash_cstring(key), (void*)expected_val))
    {
        printf("get str '%s' with expected value '%s' failed\n", key, expected_val);
        TEST_FAIL();
    }
}

static void assert_set_str(const char* key, const char* val)
{
    if (!set(hash_cstring(key), (void*)key, (void*)val))
    {
        printf("set str '%s' with value '%s' failed\n", key, val);
        TEST_FAIL();
    }
}

static void assert_get_num(intptr_t key, intptr_t expected_val)
{
    if (!get(key, (void*)expected_val))
    {
        printf("get int %ld with expected value %ld failed\n", key, expected_val);
        TEST_FAIL();
    }
}

static void assert_set_num(intptr_t key, intptr_t val)
{
    if (!set(key, (void*)key, (void*)val))
    {
        printf("set int %ld with value %ld failed\n", key, val);
        TEST_FAIL();
    }
}

static void test_set_get(void)
{
    const char* keys[] = {
        "foo", "bar", "baz", "bazz", "foot", "buzz", "jane", "x"
    };

    for (uint32_t i = 0; i < LENGTH(keys); i++)
    {
        assert_set_str(keys[i], keys[i]);
    }

    for (uint32_t i = 0; i < LENGTH(keys); i++)
    {
        assert_get_str(keys[i], keys[i]);
    }
}

static void test_set_get_alot(void)
{
    // hash table reaches a size of 721KB, very chunky
    intptr_t i, max = 1500;
    for (i = 0; i <= max; i++)
    {
        assert_set_num(i, i);
    }

    for (i = 0; i <= max; i++)
    {
        assert_get_num(i, i);
    }
}

static void test_iter(void)
{
    const char* keys[] = { "foo", "bar", "baz", "bazz", "buzz", "jane", "x" };

    for (uint32_t i = 0; i < LENGTH(keys); i++)
    {
        assert_set_str(keys[i], keys[i]);
    }

    bool seen[LENGTH(keys)] = {false};
    for (uint32_t i = 0; i < LENGTH(keys); i++)
    {
        assert_get_str(keys[i], keys[i]);
        seen[i] = true;
    }

    for (uint32_t i = 0; i < LENGTH(keys); i++)
    {
        if (!seen[i])
        {
            printf("could not find key at index %d", i);
            TEST_FAIL();
        }
    }
}

int main(void)
{
    UNITY_BEGIN();
    RUN_TEST(test_set_get);
    RUN_TEST(test_set_get_alot);
    RUN_TEST(test_iter);
    return UNITY_END();
}
