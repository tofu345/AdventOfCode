#include "../unity/unity.h"

#include "string_slice.h"

// https://www.youtube.com/watch?v=fUVvfDkDb-Y

void setUp() {}
void tearDown() {}

static void test_trim(void)
{
    string_t a = string("   tony,btw   ");
    TEST_ASSERT_TRUE(string_equal_cstring(string_trim(a), "tony,btw"));

    string_t b = string("\t   tofs,btw \n\t");
    TEST_ASSERT_TRUE(string_equal_cstring(string_trim(b), "tofs,btw"));
}

static void test_split(void)
{
    string_t s = string("arch,gentoo,fedora");

    string_t legacy_distro = string_split(&s, ',');
    TEST_ASSERT_TRUE(string_equal_cstring(legacy_distro, "arch"));
    TEST_ASSERT_TRUE(s.len > 0);

    legacy_distro = string_split(&s, ',');
    TEST_ASSERT_TRUE(string_equal_cstring(legacy_distro, "gentoo"));
    TEST_ASSERT_TRUE(s.len > 0);

    legacy_distro = string_split(&s, ',');
    TEST_ASSERT_TRUE(string_equal_cstring(legacy_distro, "fedora"));
    TEST_ASSERT_TRUE(s.len == 0);
}

int main(void)
{
    UNITY_BEGIN();
    RUN_TEST(test_trim);
    RUN_TEST(test_split);
    return UNITY_END();
}
