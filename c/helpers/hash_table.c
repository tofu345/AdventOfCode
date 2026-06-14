#include <stdlib.h>
#include <string.h>

#include "hash_table.h"

// The maximum percentage of table entries that can be filled before the table
// is grown.
#define LOAD_PERCENT 75

// The rate at which a collection's capacity grows when the size exceeds the
// current capacity.
#define GROW_FACTOR 2

hash_table_t hash_table(void)
{
    hash_table_bucket_t* buckets =
        calloc(HT_INITIAL_CAPACITY, sizeof(hash_table_bucket_t));
    if (buckets == NULL) die("could not allocate hash table entries");

    return (hash_table_t){
        .buckets = buckets,
        .capacity = HT_NUM_ENTRIES,
        .count = 0,
    };
}

static void free_overflows(hash_table_bucket_t* bucket)
{
    if (bucket && bucket->overflow) {
        free_overflows(bucket->overflow);
        free(bucket->overflow);
    }
}

void hash_table_free(hash_table_t* ht)
{
    for (size_t i = 0; i < ht->capacity; i++)
    {
        free_overflows(&ht->buckets[i]);
    }
    free(ht->buckets);
    ht->buckets = NULL;
}

uint32_t hash_cstring(const char *key)
{
    uint32_t hash = 2166136261u;
    for (const char* p = key; *p != '\0'; p++) {
        hash ^= *p;
        hash *= 16777619;
    }
    return hash;
}

uint32_t hash_string(const char* key, int len)
{
    uint32_t hash = 2166136261u;
    for (int i = 0; i < len; i++) {
        hash ^= key[i];
        hash *= 16777619;
    }
    return hash;
}

static int get_index(const hash_table_t* ht, uint32_t hash,
                     hash_table_bucket_t** bucket)
{
    size_t index = hash % ht->capacity;
    hash_table_bucket_t* b = &ht->buckets[index];

    while (b != NULL)
    {
        *bucket = b;

        for (int i = 0; i < HT_NUM_ENTRIES; i++)
        {
            if (!b->filled[i])
            {
                return i;
            }
            else if (b->hashes[i] == hash)
            {
				return i;
			}
		}

		b = b->overflow;
    }
    return -1;
}

bool hash_table_get(const hash_table_t* ht, uint32_t hash, void** dest)
{
    hash_table_bucket_t* bucket;
    int i = get_index(ht, hash, &bucket);
    if (i == -1 || !bucket->filled[i]) return false;
    *dest = bucket->entries[i].value;
    return true;
}

static bool insert(hash_table_t* ht, uint32_t hash, void* key, void* value)
{
    hash_table_bucket_t* bucket;
    int i = get_index(ht, hash, &bucket);
    if (i != -1)
    {
        if (!bucket->filled[i])
        {
            bucket->entries[i].key = key;
            bucket->hashes[i] = hash;
            bucket->filled[i] = true;
            ht->count++;
        }
        bucket->entries[i].value = value;
        return true;
    }

    hash_table_bucket_t* overflow = calloc(1, sizeof(hash_table_bucket_t));
    if (overflow == NULL) return false;

    bucket->overflow = overflow;
    overflow->entries[0].key = key;
    overflow->entries[0].value = value;
    overflow->hashes[0] = hash;
    overflow->filled[0] = true;
    ht->count++;
    return true;
}

static bool resize(hash_table_t* ht, size_t new_capacity)
{
    if (ht->capacity == new_capacity) return true;

    hash_table_bucket_t* new_buckets =
        calloc(new_capacity, sizeof(hash_table_bucket_t));
    if (new_buckets == NULL) return false;

    size_t old_capacity = ht->capacity;
    hash_table_bucket_t* old_buckets = ht->buckets;

    // iterate on [old_buckets]
    hash_table_iter_t it = hash_table_iter(ht);

    // insert in [new_buckets]
    ht->count = 0; // increased by `insert()`
    ht->buckets = new_buckets;
    ht->capacity = new_capacity;

    hash_table_entry_t* entry;
    while ((entry = hash_table_iter_next(&it)) != NULL)
    {
        insert(ht, it.cur_b->hashes[it.next_entry - 1], entry->key, entry->value);
    }

    for (size_t i = 0; i < old_capacity; i++)
    {
        free_overflows(&ht->buckets[i]);
    }
    free(old_buckets);
    return true;
}

bool hash_table_set(hash_table_t* ht, uint32_t hash, void* key, void* value)
{
    if (ht->count + 1 > ht->capacity * LOAD_PERCENT / 100)
    {
        resize(ht, ht->capacity * GROW_FACTOR);
    }

    return insert(ht, hash, key, value);
}

hash_table_iter_t hash_table_iter(hash_table_t* ht)
{
    return (hash_table_iter_t){
        .buckets = ht->buckets,
        .capacity = ht->capacity,
        .cur_b = ht->buckets,
        .cur_b_idx = 0,
        .next_entry = 0,
    };
}

hash_table_entry_t* hash_table_iter_next(hash_table_iter_t* it)
{
    while (true)
	{
		if (it->next_entry >= HT_NUM_ENTRIES || !it->cur_b->filled[it->next_entry])
		{
			it->next_entry = 0;
			if (it->cur_b->overflow != NULL)
			{
				it->cur_b = it->cur_b->overflow;
			}
			else if (it->cur_b_idx + 1 >= it->capacity)
			{
				return NULL;
			}
			else
			{
				it->cur_b = &it->buckets[++it->cur_b_idx];
			}
			continue;
		}

		return &it->cur_b->entries[it->next_entry++];
	}
}
