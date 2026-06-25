#ifndef HELPERS_HASH_TABLE_H
#define HELPERS_HASH_TABLE_H

// Generic Hash-table
//
// Based on:
// https://benhoyt.com/writings/hash-table-in-c/.
// https://dave.cheney.net/2018/05/29/how-the-go-runtime-implements-maps-efficiently-without-generics

#include <stdint.h>

#include "helpers.h"

// Number of elements in a bucket.
#define HASH_TABLE_NUM_ENTRIES 8

// Initial amount of buckets.
#ifndef HASH_TABLE_INITIAL_CAPACITY
#	define HASH_TABLE_INITIAL_CAPACITY 16
#endif

typedef struct {
    void* key;
    void* value;
} hash_table_entry_t;

// entries are continuous, if an empty entry is encountered, it is assumed
// there are no filled entries following it
typedef struct hash_table_bucket {
    bool filled[HT_NUM_ENTRIES];
    uint32_t hashes[HT_NUM_ENTRIES];
    hash_table_entry_t entries[HT_NUM_ENTRIES];
    struct hash_table_bucket* overflow;
} hash_table_bucket_t;

typedef struct {
    hash_table_bucket_t* buckets;
    size_t capacity; // number of available buckets
    size_t count; // number of filled entries
} hash_table_t;

hash_table_t hash_table(void);
void hash_table_free(hash_table_t* ht);

uint32_t hash_cstring(const char *key); // Calculate 64-bit FNV-1a hash of string.
uint32_t hash_string(const char* key, int len); // Calculate 64-bit FNV-1a hash of string.

// store value of [key] in [dest], returns true if key exists in hash table.
bool hash_table_get(const hash_table_t* ht, uint32_t hash, void** dest);

// set [key] to [value], returns true on success
bool hash_table_set(hash_table_t* ht, uint32_t hash, void* key, void* value);

// void hash_table_remove(hash_table_t* ht, uint64_t hash); // TODO

typedef struct {
    hash_table_bucket_t* buckets;
    size_t capacity;

    hash_table_bucket_t* cur_b;
    size_t cur_b_idx;

    size_t next_entry;
} hash_table_iter_t;

hash_table_iter_t hash_table_iter(hash_table_t* ht);

// retrieve the next entry or NULL.
hash_table_entry_t* hash_table_iter_next(hash_table_iter_t* it);

#endif // HELPERS_HASH_TABLE_H
