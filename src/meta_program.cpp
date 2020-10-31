// TODO:
// - Write my own lexer, and then handle preprocessor stuff properly...

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <setjmp.h>

typedef int32_t  b32;

typedef float    f32;
typedef double   f64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t   s8;
typedef int16_t  s16;
typedef int32_t  s32;
typedef int64_t  s64;

typedef uintptr_t umm;
typedef intptr_t  smm;

// NOTE: Anonymous namespace should be equivalent to sticking "static" on everything in here...
//       But of course it fucking makes compile errors and things in the debugger harder to read
//       because it sticks 'anonymous namespace':: on everything. Why do I ever bother trying to
//       do C++.
namespace {

// --LEXER DEFINITIONS--
#define STB_C_LEX_C_DECIMAL_INTS    Y   //  "0|[1-9][0-9]*"                        CLEX_intlit
#define STB_C_LEX_C_HEX_INTS        Y   //  "0x[0-9a-fA-F]+"                       CLEX_intlit
#define STB_C_LEX_C_OCTAL_INTS      Y   //  "[0-7]+"                               CLEX_intlit
#define STB_C_LEX_C_DECIMAL_FLOATS  Y   //  "[0-9]*(.[0-9]*([eE][-+]?[0-9]+)?)     CLEX_floatlit
#define STB_C_LEX_C99_HEX_FLOATS    N   //  "0x{hex}+(.{hex}*)?[pP][-+]?{hex}+     CLEX_floatlit
#define STB_C_LEX_C_IDENTIFIERS     Y   //  "[_a-zA-Z][_a-zA-Z0-9]*"               CLEX_id
#define STB_C_LEX_C_DQ_STRINGS      Y   //  double-quote-delimited strings with escapes  CLEX_dqstring
#define STB_C_LEX_C_SQ_STRINGS      N   //  single-quote-delimited strings with escapes  CLEX_ssstring
#define STB_C_LEX_C_CHARS           Y   //  single-quote-delimited character with escape CLEX_charlits
#define STB_C_LEX_C_COMMENTS        Y   //  "/* comment */"
#define STB_C_LEX_CPP_COMMENTS      Y   //  "// comment to end of line\n"
#define STB_C_LEX_C_COMPARISONS     Y   //  "==" CLEX_eq  "!=" CLEX_noteq   "<=" CLEX_lesseq  ">=" CLEX_greatereq
#define STB_C_LEX_C_LOGICAL         Y   //  "&&"  CLEX_andand   "||"  CLEX_oror
#define STB_C_LEX_C_SHIFTS          Y   //  "<<"  CLEX_shl      ">>"  CLEX_shr
#define STB_C_LEX_C_INCREMENTS      Y   //  "++"  CLEX_plusplus "--"  CLEX_minusminus
#define STB_C_LEX_C_ARROW           Y   //  "->"  CLEX_arrow
#define STB_C_LEX_EQUAL_ARROW       N   //  "=>"  CLEX_eqarrow
#define STB_C_LEX_C_BITWISEEQ       Y   //  "&="  CLEX_andeq    "|="  CLEX_oreq     "^="  CLEX_xoreq
#define STB_C_LEX_C_ARITHEQ         Y   //  "+="  CLEX_pluseq   "-="  CLEX_minuseq
                                        //  "*="  CLEX_muleq    "/="  CLEX_diveq    "%=" CLEX_modeq
                                        //  if both STB_C_LEX_SHIFTS & STB_C_LEX_ARITHEQ:
                                        //                      "<<=" CLEX_shleq    ">>=" CLEX_shreq

#define STB_C_LEX_PARSE_SUFFIXES    N   // letters after numbers are parsed as part of those numbers, and must be in suffix list below
#define STB_C_LEX_DECIMAL_SUFFIXES  ""  // decimal integer suffixes e.g. "uUlL" -- these are returned as-is in string storage
#define STB_C_LEX_HEX_SUFFIXES      ""  // e.g. "uUlL"
#define STB_C_LEX_OCTAL_SUFFIXES    ""  // e.g. "uUlL"
#define STB_C_LEX_FLOAT_SUFFIXES    ""  //

#define STB_C_LEX_0_IS_EOF             Y  // if Y, ends parsing at '\0'; if N, returns '\0' as token
#define STB_C_LEX_INTEGERS_AS_DOUBLES  N  // parses integers as doubles so they can be larger than 'int', but only if STB_C_LEX_STDLIB==N
#define STB_C_LEX_MULTILINE_DSTRINGS   N  // allow newlines in double-quoted strings
#define STB_C_LEX_MULTILINE_SSTRINGS   N  // allow newlines in single-quoted strings
#define STB_C_LEX_USE_STDLIB           Y  // use strtod,strtol for parsing #s; otherwise inaccurate hack
#define STB_C_LEX_DOLLAR_IDENTIFIER    N  // allow $ as an identifier character
#define STB_C_LEX_FLOAT_NO_DECIMAL     Y  // allow floats that have no decimal point if they have an exponent

#define STB_C_LEX_DEFINE_ALL_TOKEN_NAMES  N   // if Y, all CLEX_ token names are defined, even if never returned
                                              // leaving it as N should help you catch config bugs

#define STB_C_LEX_DISCARD_PREPROCESSOR    Y   // discard C-preprocessor directives (e.g. after prepocess
                                              // still have #line, #pragma, etc)

//#define STB_C_LEX_ISWHITE(str)    ... // return length in bytes of whitespace characters if first char is whitespace

#define STB_C_LEXER_DEFINITIONS         // This line prevents the header file from replacing your definitions
// --END--

#define STB_C_LEXER_IMPLEMENTATION
#include "external/stb_c_lexer.h"

//
// NOTE: Test code
//

char alt_input[] = R"CODE(
struct TestA {
    int an_int;
    struct Nested {
        int this_is_one_nested_type;
    } nested;
};

class TestB {
    int another_int;
    enum Nested {
        This, Is, Another, Nested, Type,
    } nested;

public:
    struct {
        int shhhh;
    } anonymous_member;

    union {
        struct {
            short completely_anonymous;
            short very_mush_so;
        };
        int unions_are_cool_btw;
    };
};
)CODE";

char input[] = R"CODE(
struct Path {
    u32 count, bounty;
    u32 flags;
private:
    f32 total_cost;
    Step* steps, step;

    struct Test {
        int i_am_not_the_other_test;
    };

    // These two are equivalent from the point of view of the type info

    struct TestA {
        int z;
        int x;
    } test_a;

    struct TestB {
        int z;
        int x;
    };

    TestB test_b;

    // These two are also equivalent from the point of view of type info

    int separate_x;
    int separate_y;

    struct {
        int bundled_x;
        int bundled_y;
    };

    // These have different offsets, however we can generate code to have the compiler tell us their offsets. We do not care as our metaprogram is concerned.
    // So these are equivalent too.
    
    union {
        int union_z;
        int union_w;
    };

    int regular_z;
    int regular_w;

    // What about these? Again, the difference is the memory layout, not the type info. Therefore these are equivalent!

    union {
        struct {
            short a;
            short b;
        };
        int c;
    };

    short d;
    short e;
    int f;

    // But how do we represent the following?

    struct {
        int p;
        int q;
    } anonymous;

    // Fundamentally, this is a question of how we store the type of a member.
    // How do we expect to use this?
    // Let's jump out of the string... (1)
};

class Baguette;

class Baguette {
public:
    int hello;
private:
    int goodbye;

    struct Path* b;

    enum Test {
        A, B, C,
    } test;

    struct {
        int y = 0;
    } anon;
};
)CODE";

// (1) Down here.
// Usage code for a code generator using this info can reasonably be expected to look something like this:
// struct AStruct;
// This would just be a compile time overload, not really useful.
// StructTypeInfo* info = get_type_info(a_struct);
// We could do this instead:
// StructTypeInfo* info = TypeInfo::AStruct;
// Or if we're targetting C:
// StructTypeInfo* info = TypeInfo_AStruct; // Could use a macro instead
// for (int i = 0; i < info->member_count; ++i) {
//     MemberInfo* member = info->members + i;
//     TypeInfo* member_type = member->type_info;
//     printf("Member type name: %s\n", member_type->name);
//     if (member_type->kind == TypeInfoKind_Struct) {
//         StructTypeInfo* member_info = (StructTypeInfo*)member; // union or "inheritance"
//         // do something with it?
//     }
//     if (member_type->type == get_type(char*)) {
//         char** member_data = (char**)get_member_ptr(a_struct, member->offset);
//         printf("a_struct->%s: "%s"\n", member->name, *member_data);
//     }
// }
// Seeing the above, the anonymous struct type info just needs to exist _somewhere_, and a pointer to it
// needs to be put into the MemberInfo struct.
// However, what is the type enum? Is there one?
// What if we don't want to store the pointer, just the enum, is that tractable?
// Or do we ditch the notion that we'll use enums, and will we just compare type info pointers?
// But this leaves us with the question, what about types we don't have a declaration for, and we are only
// talking about because we want to test for equality with member types?

//
// NOTE: Defines
//

#define Assert(x) assert(x)

#define ForArray(array) \
    for (auto it = (array)->data, it_end = (array)->data + (array)->count; it < it_end; ++it)

#define ForBlockArray(array) \
    for (auto block = (array)->first_block; block; block = block->next) \
    for (auto it = block->data, it_end = it + block->count; it < it_end; ++it)

#define INVALID_CODE_PATH Assert(!"Invalid Code Path");
#define INVALID_DEFAULT_CASE default: { Assert(!"Invalid Default Case"); } break;
#define NOT_IMPLEMENTED Assert(!"Not Implemented");

#define ArrayCount(array) (sizeof(array) / sizeof((array)[0]))
#define ArrayEnd(array) (array) + ArrayCount(array)
// #define ForArray(it, array) for (auto it = array; it < ArrayEnd(array); ++it)

#define SllStackPush(h, n) ((n)->next = (h), (h) = (n))
#define SllStackPop_(h) ((h) = (h)->next)
#define SllStackPop(h) h; SllStackPop_(h)
#define SllQueuePush(f, l, n) ((n)->next = 0, ((f) ? (l)->next = (n) : (f) = (n)), (l) = (n))
#define SllQueuePop(f, l, n) f; (SllStackPop_(f), ((f) ? 0 : (l) = 0))

#define ForSllUnique(it, head, next) for (auto* it = head; it; it = it->next)
#define ForSll(it, head) ForSllUnique(it, head, next)
#define ForSllOuterUnique(it_at, head, next) for (auto** it_at = &(head); *it_at; it_at = &(*it_at)->next)
#define ForSllOuter(it_at, head) ForSllOuterUnique(it_at, head, next)

#define DllInit(s) ((s)->next = s, (s)->prev = s)
#define DllInsertFront(h, n) ((n)->next = (h)->next, (n)->prev = h, (n)->next->prev = n, (n)->prev->next = n)
#define DllInsertBack(h, n) ((n)->next = h, (n)->prev = (h)->prev, (n)->next->prev = n, (n)->prev->next = n)
#define DllRemove(n) ((n)->next->prev = (n)->prev, (n)->prev->next = (n)->next)
#define DllIsEmpty(s) ((s)->next == (s))

#define ForDllUnique(it, sentinel, next, prev) for (auto prev_##it = (sentinel), it = (sentinel)->next; \
                                                    it != (sentinel);                                   \
                                                    prev_##it = prev_##it->next, it = prev_##it->next)
#define ForDll(it, sentinel) ForDllUnique(it, sentinel, next, prev)

//
// NOTE: Memory management and data structures
//

inline void* heap_alloc(umm size, b32 clear = true) {
    void* result = malloc(size);
    if (clear) {
        memset(result, 0, size);
    }
    return result;
}

inline void heap_free(void* ptr) {
    if (ptr) {
        free(ptr);
    }
}

inline void* heap_realloc(void* old_ptr, umm size) {
    void* result = realloc(old_ptr, size);
    return result;
}

#define alloc_struct(type, ...) (type*)heap_alloc(sizeof(type), ##__VA_ARGS__)
#define alloc_array(n, type, ...) (type*)heap_alloc(n*sizeof(type), ##__VA_ARGS__)
#define zero_struct(s) memset(s, 0, sizeof(*s))

inline char* alloc_copy_string(umm len, char* src) {
    char* dst = alloc_array(len + 1, char, false);
    memcpy(dst, src, len);
    dst[len] = 0;
    return dst;
}

template <typename T>
struct Array {
    u32 capacity;
    u32 count;
    T* data;

    //

    T& operator[](u32 index) {
        Assert(index < count);
        return data[index];
    }
};

template <typename T>
void ensure_space(Array<T>* array, u32 count) {
    if (array->capacity < (array->count + count)) {
        if (!array->capacity) {
            array->capacity = 8;
        } else {
            array->capacity *= 2;
        }
        array->data = (T*)heap_realloc(array->data, sizeof(T)*array->capacity);
    }
}

template <typename T>
T* array_add(Array<T>* array, T* value = 0) {
    ensure_space(array, 1);
    T* result = array->data + array->count++;
    if (value) {
        *result = *value;
    } else {
        *result = {};
    }
    return result;
}

template <typename T>
void free_array(Array<T>* array) {
    heap_free(array->data);
    zero_struct(array);
}

// NOTE: This is almost more like an allocator, I probably just want arenas.
template <typename T>
struct BlockArray {
    static constexpr int BLOCK_CAPACITY = 32;

    struct Block {
        Block* next;

        umm count;
        T data[BLOCK_CAPACITY];
    };

    Block* first_block;
    Block* last_block;
};

template <typename T>
inline T* array_add(BlockArray<T>* array, T* value = 0) {
    if (!array->first_block ||
        (array->first_block->count == array->BLOCK_CAPACITY))
    {
        auto new_block = alloc_struct(typename BlockArray<T>::Block);
        if (array->first_block) {
            array->last_block = array->last_block->next = new_block;
        } else {
            array->first_block = array->last_block = new_block;
        }
    }

    auto block = array->last_block;
    T* result = &block->data[block->count++];
    if (value) {
        *result = *value;
    }

    return result;
}

template <typename T>
inline void free_array(BlockArray<T>* array) {
    while (array->first_block) {
        auto block = SllStackPop(array->first_block);
        heap_free(block);
    }
    zero_struct(array);
}

struct HashTable {
    static constexpr u64 TOMBSTONE = 0xFFFFFFFFFFFFFFFF;
    static constexpr u32 STARTING_CAPACITY = 16; // TODO: Make this variable by letting the user just set capacity before using the table

    u32 capacity;
    u32 count;

    u64* keys;
    u64* vals;
};

void resize_table(HashTable* table, u32 new_capacity) {
    if (new_capacity > table->capacity) {
        u32 old_capacity = table->capacity;
        u64* old_keys = table->keys;
        u64* old_vals = table->vals;

        table->capacity = new_capacity;
        table->keys = alloc_array(table->capacity, u64);
        table->vals = alloc_array(table->capacity, u64);

        if (old_capacity) {
            for (u32 index = 0; index < old_capacity; ++index) {
                u64 old_key = old_keys[index];
                u64 old_val = old_vals[index];
                if (old_key) {
                    u32 slot = old_key % table->capacity;
                    table->keys[slot] = old_key;
                    table->vals[slot] = old_val;
                }
            }

            heap_free(old_keys);
            heap_free(old_vals);
        }
    }
}

void table_insert(HashTable* table, u64 key, u64 val) {
    if (3*(u64)table->capacity < 4*(u64)(table->count + 1)) {
        resize_table(table, (table->count > table->STARTING_CAPACITY ? table->count : table->STARTING_CAPACITY));
    }

    if (!key) {
        ++key; // NOTE: Not very elegant, but you get the point. Can't have a null key.
    } else if (key == table->TOMBSTONE) {
        --key; // NOTE: Not very elegant, but you get the point. Can't have a tombstone key.
    }

    u64 slot = key % table->capacity;
    for (u32 i = 0; i < table->capacity; ++i) {
        u64 slot = (key + i) % table->capacity;
        if (!table->keys[slot] || table->keys[slot] == table->TOMBSTONE) {
            ++table->count;
            table->keys[slot] = key;
            table->vals[slot] = val;
            break;
        } else if (table->keys[slot] == key) {
            // NOTE: Hash collision! These aren't handled by this table, so now your program is wrong somehow. Sorry.
            //       In debug, let's assert:
            INVALID_CODE_PATH;
        }
    }
}

b32 table_lookup(HashTable* table, u64 key, u64* val) {
    b32 result = false;

    if (table->capacity) {
        if (!key) {
            ++key; // NOTE: Not very elegant, but you get the point. Can't have a null key.
        } else if (key == table->TOMBSTONE) {
            --key; // NOTE: Not very elegant, but you get the point. Can't have a tombstone key.
        }

        for (u32 i = 0; i < table->capacity; ++i) {
            u64 slot = (key + i) % table->capacity;
            if (table->keys[slot]) {
                if (table->keys[slot] == key) {
                    result = true;
                    *val = table->vals[slot];
                    break;
                }
            } else {
                break;
            }
        }
    }

    return result;
}

// TODO: table_remove

void free_table(HashTable* table) {
    if (table->capacity) {
        heap_free(table->keys);
        heap_free(table->vals);
        zero_struct(table);
    }
}

//
// NOTE: Utility
//

u32 toggle_flag(u32 flags, u32 flag, b32 toggle) {
    if (toggle) {
        flags |= flag;
    } else {
        flags &= ~flag;
    }
    return flags;
}

b32 read_entire_file_null_terminated(char* file_name, u32* out_file_size, char** out_data) {
    b32 result = false;
    FILE* file = fopen(file_name, "rb");
    if (file) {
        fseek(file, 0, SEEK_END);
        u32 file_size = ftell(file);
        fseek(file, 0, SEEK_SET);

        char* data = (char*)malloc(file_size + 1);

        if (data) {
            result = true;

            fread(data, file_size, 1, file);
            data[file_size] = 0;

            *out_file_size = file_size;
            *out_data = data;
        }

        fclose(file);
    }
    return result;
}

inline b32 strings_are_equal(char* a, char* b) {
    b32 result = ((a == b) || (0 == strcmp(a, b)));
    return result;
}

u64 hash_string(char* string, u64* out_len = 0) {
    // fnv-1a
    u64 len = 0;
    u64 hash = 0xcbf29ce484222325ull;
    while (string[len]) {
        hash ^= string[len++];
        hash *= 0x100000001b3ull;
    }
    if (out_len) *out_len = len;
    return hash;
}

//
// NOTE: String Interning
//

struct Intern {
    char* s;
};

umm intern_strings_capacity;
umm intern_strings_count;
Intern* intern_strings;

Intern null_string;

inline b32 interned_strings_are_equal(Intern a, Intern b) {
    b32 result = (a.s == b.s);
    return result;
}

Intern* get_intern_string_slot(char* string, umm* out_len = 0) {
    Intern* result = 0;

    u64 len;
    u64 hash = hash_string(string, &len);
    for (u64 i = 0; i < intern_strings_capacity; ++i) {
        u64 slot_i = (hash + i) % intern_strings_capacity;
        Intern* slot = intern_strings + slot_i;
        if (slot->s) {
            if (strings_are_equal(slot->s, string)) {
                result = slot;
            }
        } else {
            result = slot;
        }

        if (result) break;
    }

    if (out_len) *out_len = len;

    return result;
}

Intern intern_string(char* string) {
    static b32 initialized_null_string = false;
    if (!initialized_null_string) {
        initialized_null_string = true;
        null_string = intern_string("(null string)");
    }

    Intern result = null_string;
    if (string) {
        if (!intern_strings_capacity) intern_strings_capacity = 512;
        if (!intern_strings) {
            intern_strings = alloc_array(intern_strings_capacity, Intern);
        }

        // NOTE: Grow when 75% full
        if (4*intern_strings_count >= 3*intern_strings_capacity) {
            u64 old_capacity = intern_strings_capacity;
            Intern* old_strings = intern_strings;

            intern_strings_capacity *= 2;
            intern_strings = alloc_array(intern_strings_capacity, Intern);
            Assert(intern_strings);

            for (u64 i = 0; i < old_capacity; ++i) {
                Intern old_string = old_strings[i];
                if (old_string.s) {
                    *get_intern_string_slot(old_string.s) = old_string;
                }
            }

            heap_free(old_strings);
        }

        u64 len;
        Intern* slot = get_intern_string_slot(string, &len);
        if (!slot->s) {
            ++intern_strings_count;
            slot->s = alloc_copy_string(len, string);
        }

        result = *slot;
    }

    return result;
}

//
// NOTE: Syntax Nodes
//

struct Decl;

enum MemberFlag {
    MemberFlag_IsPointer  = 0x1,
    MemberFlag_IsPrivate  = 0x2,
    MemberFlag_IsVolatile = 0x4,
    MemberFlag_IsConst    = 0x8,
};

struct MemberDef {
    u32 flags;
    Decl* type;
    Intern name;
};

struct Namespace {
    Namespace* next_in_stack;

    HashTable decl_table;
    Array<Decl*> decls;
};

enum DeclarationFlag {
    DeclFlag_Anonymous = 0x2,
};

enum DeclKind {
    Decl_Stub,
    Decl_Struct,
    Decl_Class,
    Decl_Union,
    Decl_Enum,
    Decl_TypeDef,
};

inline b32 is_struct_type(DeclKind kind) {
    b32 result = ((kind == Decl_Struct) ||
                  (kind == Decl_Class)  ||
                  (kind == Decl_Union));
    return result;
}

struct Decl {
    Decl* next;

    // TODO: Eventually compact into union?
    //       Though Ryan Fleury & co seem to be into just big phat mega nodes,
    //       keeping all members for all types. Maybe there's something to be
    //       said for that.

    DeclKind kind;
    u32 flags;

    Intern name;

    Namespace ns;
    Array<MemberDef> members;

    Decl* typedef_src;
};

//
// NOTE: Parser
//

struct Parser {
    stb_lexer lex;

    char* file_name;

    char* error_loc_first_char;
    char* error_loc_last_char;

    jmp_buf error_jmp;

    Namespace global_namespace;
    Namespace* first_namespace;

    BlockArray<Decl> decl_store;
    Decl* first_free_decl;

    Decl* top_level_decl_being_parsed;
};

void init_parser(Parser* parser, char* file_name, char* input) {
    zero_struct(parser);

    stb_c_lexer_init(&parser->lex, input, 0, alloc_array(512, char, false), 512);
    stb_c_lexer_get_token(&parser->lex); // Warm up with the initial token

    parser->file_name = file_name;
    parser->first_namespace = &parser->global_namespace;
}

char* get_token_name(long token) {
    static char buffer[3];

    switch (token) {
        case CLEX_id        : { snprintf(buffer, sizeof(buffer), "id"); } break;
        case CLEX_eq        : { snprintf(buffer, sizeof(buffer), "=="); } break;
        case CLEX_noteq     : { snprintf(buffer, sizeof(buffer), "!="); } break;
        case CLEX_lesseq    : { snprintf(buffer, sizeof(buffer), "<="); } break;
        case CLEX_greatereq : { snprintf(buffer, sizeof(buffer), ">="); } break;
        case CLEX_andand    : { snprintf(buffer, sizeof(buffer), "&&"); } break;
        case CLEX_oror      : { snprintf(buffer, sizeof(buffer), "||"); } break;
        case CLEX_shl       : { snprintf(buffer, sizeof(buffer), "<<"); } break;
        case CLEX_shr       : { snprintf(buffer, sizeof(buffer), ">>"); } break;
        case CLEX_plusplus  : { snprintf(buffer, sizeof(buffer), "++"); } break;
        case CLEX_minusminus: { snprintf(buffer, sizeof(buffer), "--"); } break;
        case CLEX_arrow     : { snprintf(buffer, sizeof(buffer), "->"); } break;
        case CLEX_andeq     : { snprintf(buffer, sizeof(buffer), "&="); } break;
        case CLEX_oreq      : { snprintf(buffer, sizeof(buffer), "|="); } break;
        case CLEX_xoreq     : { snprintf(buffer, sizeof(buffer), "^="); } break;
        case CLEX_pluseq    : { snprintf(buffer, sizeof(buffer), "+="); } break;
        case CLEX_minuseq   : { snprintf(buffer, sizeof(buffer), "-="); } break;
        case CLEX_muleq     : { snprintf(buffer, sizeof(buffer), "*="); } break;
        case CLEX_diveq     : { snprintf(buffer, sizeof(buffer), "/="); } break;
        case CLEX_modeq     : { snprintf(buffer, sizeof(buffer), "%%="); } break;
        case CLEX_shleq     : { snprintf(buffer, sizeof(buffer), "<<="); } break;
        case CLEX_shreq     : { snprintf(buffer, sizeof(buffer), ">>="); } break;
        default: {
            if (token >= 0 && token <= 256) {
                snprintf(buffer, sizeof(buffer), "%c", (int)token);
            }
        } break;
    }

    return buffer;
}

enum DiagnosticKind {
    Diagnostic_Note,
    Diagnostic_Warning,
    Diagnostic_Error,
};

void print_diagnostic_va(Parser* parser, b32 print_code, DiagnosticKind kind, char* fmt, va_list args) {
    char* p = parser->lex.input_stream;
    char* where = parser->error_loc_first_char;

    char* line_text = p;

    int line_number = 1;
    int char_offset = 0;

    while (*p && (p < where)) {
        if ((*p == '\n') || (*p == '\r')) {
            p += (p[0] + p[1] == '\n' + '\r' ? 2 : 1);
            ++line_number;
            char_offset = 0;
            line_text = p;
        } else {
            ++p;
            ++char_offset;
        }
    }

    u32 line_length = 0;
    while (line_text[line_length]) {
        if ((line_text[line_length] == '\n') || (line_text[line_length] == '\r')) {
            break;
        } else {
            ++line_length;
        }
    }

    int underline_start = parser->error_loc_first_char - line_text;
    int underline_end   = parser->error_loc_last_char  - line_text;
    int underline_range = underline_end - underline_start;

    char* diagnostic_name = 0;
    switch (kind) {
        case Diagnostic_Note   : { diagnostic_name = "NOTE"; } break;
        case Diagnostic_Warning: { diagnostic_name = "WARNING"; } break;
        case Diagnostic_Error  : { diagnostic_name = "ERROR"; } break;
        INVALID_DEFAULT_CASE;
    }

    fprintf(stderr, "%s (%s:%d:%d): ", diagnostic_name, parser->file_name, line_number, char_offset);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    if (print_code) {
        fprintf(stderr, "    %.*s\n", line_length, line_text);
        for (int i = 0; i < underline_start + 4; ++i) {
            fprintf(stderr, " ");
        }
        for (int i = 0; i < underline_range; ++i) {
            fprintf(stderr, "~");
        }
        fprintf(stderr, "^\n");
    }
    fprintf(stderr, "\n");
}

void note(Parser* parser, char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    print_diagnostic_va(parser, false, Diagnostic_Note, fmt, args);
    va_end(args);
}

void note_at_token(Parser* parser, char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    print_diagnostic_va(parser, true, Diagnostic_Note, fmt, args);
    va_end(args);
}

void warn(Parser* parser, char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    print_diagnostic_va(parser, false, Diagnostic_Warning, fmt, args);
    va_end(args);
}

void warn_at_token(Parser* parser, char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    print_diagnostic_va(parser, true, Diagnostic_Warning, fmt, args);
    va_end(args);
}

void error(Parser* parser, char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    print_diagnostic_va(parser, false, Diagnostic_Error, fmt, args);
    va_end(args);

    longjmp(parser->error_jmp, 1);
}

void error_at_token(Parser* parser, char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    print_diagnostic_va(parser, true, Diagnostic_Error, fmt, args);
    va_end(args);

    longjmp(parser->error_jmp, 1);
}

void push_namespace(Parser* parser, Namespace* ns) {
    ns->next_in_stack = parser->first_namespace;
    parser->first_namespace = ns;
}

void pop_namespace(Parser* parser) {
    Assert(parser->first_namespace != &parser->global_namespace); // Don't pop the global stack!
    parser->first_namespace = parser->first_namespace->next_in_stack;
}

void free_decl(Parser* parser, Decl* decl) {
    free_array(&decl->members);
    SllStackPush(parser->first_free_decl, decl);
}

void free_namespace_recursively(Parser* parser, Namespace* ns) {
    for (u32 decl_index = 0; decl_index < ns->decls.count; ++decl_index) {
        Decl* decl = ns->decls[decl_index];
        if (decl->ns.decls.count) {
            free_namespace_recursively(parser, &decl->ns);
        }
        free_decl(parser, decl);
    }
    free_table(&ns->decl_table);
    free_array(&ns->decls);
}

Decl* add_decl(Parser* parser) {
    if (!parser->first_free_decl) {
        parser->first_free_decl = array_add(&parser->decl_store);
    }

    Decl* result = parser->first_free_decl;
    parser->first_free_decl = result->next;

    return result;
}

Decl* remove_last_top_level_decl_and_free_children(Parser* parser) {
    Decl* to_free = parser->top_level_decl_being_parsed;
    parser->top_level_decl_being_parsed = 0;

    if (to_free) {
        free_decl(parser, to_free);
    }

    return to_free;
}

Decl* get_type_by_name_internal(Parser* parser, Namespace* first_ns, Intern name, b32 is_declaration) {
    Decl* result = 0;

    u64 hash = hash_string(name.s);
    for (Namespace* ns = first_ns; ns; ns = ns->next_in_stack) {
        b32 will_allocate = (is_declaration || !ns->next_in_stack);
        if ((ns->decls.count > 0) || will_allocate) {
            u64 type_index;
            if (table_lookup(&ns->decl_table, hash, &type_index)) {
                result = ns->decls[type_index];
            } else if (will_allocate) {
                result = add_decl(parser);
                result->name = name;
                array_add(&ns->decls, &result);
                table_insert(&ns->decl_table, hash, ns->decls.count - 1);
                if (is_declaration && (ns == &parser->global_namespace)) {
                    parser->top_level_decl_being_parsed = result;
                }
            }
        }
    }

    return result;
}

Decl* get_type_by_name(Parser* parser, Intern name) {
    Decl* result = get_type_by_name_internal(parser, parser->first_namespace, name, false);
    return result;
}

Decl* declare_type_by_name(Parser* parser, Intern name) {
    Decl* result = get_type_by_name_internal(parser, parser->first_namespace, name, true);
    return result;
}

Decl* add_anonymous_declaration(Parser* parser) {
    Decl* result = add_decl(parser);
    result->flags |= DeclFlag_Anonymous;
    return result;
}

b32 next_token(Parser* parser) {
    parser->error_loc_first_char = parser->lex.where_firstchar;
    parser->error_loc_last_char  = parser->lex.where_lastchar;
    return stb_c_lexer_get_token(&parser->lex);
}

long peek_token(Parser* parser) {
    return parser->lex.token;
}

b32 consume_token(Parser* parser, long token) {
    b32 result = false;
    if (peek_token(parser) == token) {
        result = true;
        next_token(parser);
    } 
    return result;
}

b32 seek_token(Parser* parser, long token) {
    b32 result = false;
    while (peek_token(parser) != CLEX_eof) {
        if (peek_token(parser) == token) {
            result = true;
            break;
        }
        next_token(parser);
    }
    return result;
}

b32 seek_and_consume_token(Parser* parser, long token) {
    b32 result = false;
    while (peek_token(parser) != CLEX_eof) {
        if (consume_token(parser, token)) {
            result = true;
            break;
        }
        next_token(parser);
    }
    return result;
}

b32 seek_and_consume_balanced_token_pair(Parser* parser, long opening_token, long closing_token) {
    b32 result = false;

    u32 depth = 0;
    while (peek_token(parser) != CLEX_eof) {
        if (consume_token(parser, opening_token)) {
            ++depth;
        } else if (consume_token(parser, closing_token)) {
            --depth;
        } else if (depth > 0) {
            next_token(parser);
        } else {
            break;
        }
    }

    return result;
}

void expect_token(Parser* parser, long token) {
    if (!consume_token(parser, token)) {
        error_at_token(parser, "Expected token '%s'", get_token_name(token));
    }
}

b32 match_id(Parser* parser, char* id) {
    b32 result = false;
    if (0 == strcmp(parser->lex.string, id)) {
        result = true;
        next_token(parser);
    }
    return result;
}

b32 consume_id(Parser* parser, Intern* out_id = 0) {
    b32 result = false;
    if (peek_token(parser) == CLEX_id) {
        result = true;
        if (out_id) *out_id = intern_string(parser->lex.string);
        next_token(parser);
    }
    return result;
}

Intern expect_id(Parser* parser) {
    Intern result = {};
    if (peek_token(parser) == CLEX_id) {
        result = intern_string(parser->lex.string);
        next_token(parser);
    } else {
        error_at_token(parser, "Expected identifier.");
    }
    return result;
}

u32 parse_member_keywords(Parser* parser) {
    u32 result = 0;
    if (match_id(parser, "volatile")) {
        result |= MemberFlag_IsVolatile;
    } else if (match_id(parser, "const")) {
        // TODO: pointer to const versus const pointer?
        result |= MemberFlag_IsConst;
    }
    return result;
}

Decl* parse_struct(Parser* parser, DeclKind kind);
Decl* parse_enum(Parser* parser);
Decl* parse_typedef(Parser* parser);
Decl* parse_using(Parser* parser);

Decl* parse_type(Parser* parser) {
    Decl* result = 0;

    if (peek_token(parser) == CLEX_id) {
        if (match_id(parser, "struct")) {
            result = parse_struct(parser, Decl_Struct);
        } else if (match_id(parser, "class")) {
            result = parse_struct(parser, Decl_Class);
        } else if (match_id(parser, "union")) {
            result = parse_struct(parser, Decl_Union);
        } else if (match_id(parser, "enum")) {
            result = parse_enum(parser);
        } else if (match_id(parser, "typedef")) {
            result = parse_typedef(parser);
        } else if (match_id(parser, "using")) {
            result = parse_using(parser);
        }
    }

    return result;
}

Decl* parse_struct(Parser* parser, DeclKind kind) {
    Assert(is_struct_type(kind));

    Decl* type = 0;

    Intern name;
    if (consume_id(parser, &name)) {
        if (peek_token(parser) != '{') {
            type = get_type_by_name(parser, name);    
        } else {
            type = declare_type_by_name(parser, name);    
        }
    } else {
        type = add_anonymous_declaration(parser);
        switch (kind) {
            case Decl_Struct: { type->name = intern_string("(anonymous struct)"); } break;
            case Decl_Class: { type->name = intern_string("(anonymous class)"); } break;
            case Decl_Union: { type->name = intern_string("(anonymous union)"); } break;
            INVALID_DEFAULT_CASE;
        }
    }

    type->kind = kind;

    if (consume_token(parser, '{')) {
        push_namespace(parser, &type->ns);

        b32 is_private = (type->kind == Decl_Class);
        while (!consume_token(parser, '}')) {
            if (match_id(parser, "public")) {
                expect_token(parser, ':');
                is_private = false;
            }

            if (match_id(parser, "private")) {
                expect_token(parser, ':');
                is_private = true;
            }

            u32 member_flags = 0;

            b32 type_was_declared_locally = true;
            Decl* member_type = parse_type(parser);
            if (!member_type) {
                type_was_declared_locally = false;

                member_flags |= parse_member_keywords(parser);

                Intern member_type_name;
                if (consume_id(parser, &member_type_name)) {
                    member_type = get_type_by_name(parser, member_type_name);
                    member_flags |= parse_member_keywords(parser);
                } else {
                    error_at_token(parser, "Expected type for member declaration.");
                }
            }

            if (member_type) {
                if (consume_token(parser, '<')) {
                    u32 template_depth = 1;
                    while (template_depth > 0) {
                        if (peek_token(parser) == '<') ++template_depth;
                        if (peek_token(parser) == '>') --template_depth;
                        next_token(parser);
                    }
                }

                b32 has_instances = false;
                do {
                    b32 is_pointer = consume_token(parser, '*');
                    b32 is_ref     = consume_token(parser, '&'); // NOTE: If this is the case, it must be a member function right? 
                                                                 //       I don't think you can have a regular reference member.
                    Intern member_name;
                    if (consume_id(parser, &member_name)) {
                        if ((peek_token(parser) != '(') &&
                            (!match_id(parser, "operator")))
                        {
                            has_instances = true;

                            MemberDef* member = array_add(&type->members);
                            member->flags |= member_flags;

                            if (is_private) member->flags |= MemberFlag_IsPrivate;

                            member->type = member_type;
                            member->name = member_name;
                            if (is_pointer) member->flags |= MemberFlag_IsPointer;

                            while (consume_token(parser, '[')) {
                                seek_and_consume_token(parser, ']');
                            }

                            if (consume_token(parser, '=')) {
                                seek_token(parser, ';');
                            }
                        } else {
                            warn_at_token(parser, "Member functions / operator overloads are not (yet) indexed.");

                            seek_and_consume_token(parser, ')');
                            match_id(parser, "const");
                            if (peek_token(parser) == '{') {
                                seek_and_consume_balanced_token_pair(parser, '{', '}');
                            }
                        }
                    } else {
                        if (!type_was_declared_locally) {
                            error_at_token(parser, "Expected identifier for member declaration.");
                        }
                    }
                } while (consume_token(parser, ','));

                // TODO: This will now not spot a missing ;, but does that really matter? The compiler would.
                consume_token(parser, ';');

                // NOTE: Copy out the members of fully anonymous struct declarations.
                // TODO: Seems a bit inelegant.
                if (!has_instances && (member_type->flags & DeclFlag_Anonymous)) {
                    ForArray (&member_type->members) {
                        MemberDef member = *it;
                        member.flags = toggle_flag(member.flags, MemberFlag_IsPrivate, is_private);
                        array_add(&type->members, &member);
                    }
                }
            }
        }

        pop_namespace(parser);
    }

    return type;
}

Decl* parse_enum(Parser* parser) {
    Decl* type = 0;

    Intern name;
    if (consume_id(parser, &name)) {
        if (peek_token(parser) != '{') {
            type = get_type_by_name(parser, name);    
        } else {
            type = declare_type_by_name(parser, name);    
        }
    } else {
        type = add_anonymous_declaration(parser);
        type->name = intern_string("(anonymous enum)");
    }

    type->kind = Decl_Enum;

    Intern value_type_name = intern_string("int");
    if (consume_token(parser, ':')) {
        value_type_name = expect_id(parser);
    }
    Decl* value_type = get_type_by_name(parser, value_type_name);

    if (consume_token(parser, '{')) {
        while (!consume_token(parser, '}')) {
            MemberDef* member = array_add(&type->members);
            member->type = value_type;
            member->name = expect_id(parser);
            while (peek_token(parser) != CLEX_eof && !consume_token(parser, ',')) {
                next_token(parser);
            }
        }
    }

    return type;
}

Decl* parse_typedef(Parser* parser) {
    Decl* type = 0;

    // TODO: What about "unsigned int"? We need a parse_typename kind of thing
    Intern src_type_name = expect_id(parser);
    Intern dst_type_name = expect_id(parser);

    Decl* src_type = get_type_by_name(parser, src_type_name);
    Decl* dst_type = declare_type_by_name(parser, dst_type_name);

    dst_type->kind = Decl_TypeDef;
    dst_type->typedef_src = src_type;

    return type;
}

Decl* parse_using(Parser* parser) {
    Decl* type = 0;

    if (match_id(parser, "namespace")) {
        warn_at_token(parser, "using namespaces is not yet recognised by the parser.");
    } else {
        Intern dst_type_name = expect_id(parser);
        expect_token(parser, '=');
        // TODO: What about "unsigned int"? We need a parse_typename kind of thing
        Intern src_type_name = expect_id(parser);

        Decl* src_type = get_type_by_name(parser, src_type_name);
        Decl* dst_type = declare_type_by_name(parser, dst_type_name);

        dst_type->kind = Decl_TypeDef;
        dst_type->typedef_src = src_type;
    }

    return type;
}

//
// NOTE: Output
//

void print_line_va(int depth, char* fmt, va_list args) {
    for (int i = 0; i < depth; ++i) {
        printf("    ");
    }
    vprintf(fmt, args);
}

void print_line(int depth, char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    print_line_va(depth, fmt, args);
    va_end(args);
}

void print_type(Decl* type, int depth = 0) {
    switch (type->kind) {
        case Decl_Struct:
        case Decl_Class:
        case Decl_Union: {
            char* keyword = 0;
            if (type->kind == Decl_Struct) keyword = "struct";
            if (type->kind == Decl_Class)  keyword = "class";
            if (type->kind == Decl_Union)  keyword = "union";

            b32 is_private = (type->kind == Decl_Class);

            print_line(depth, "%s %s (0x%llX) {\n", keyword, type->name.s, (umm)type);

            ++depth;
            ForArray (&type->members) {
                b32 member_is_private = it->flags & MemberFlag_IsPrivate;
                if (is_private != member_is_private) {
                    is_private  = member_is_private;
                    printf("%s:\n", is_private ? "private" : "public");
                }
                print_line(depth, "%s%s%s (0x%llX) %s%s;\n",
                    (it->flags & MemberFlag_IsConst ? "const " : ""),
                    (it->flags & MemberFlag_IsVolatile ? "volatile " : ""),
                    it->type->name,
                    (umm)it->type,
                    (it->flags & MemberFlag_IsPointer ? "*" : ""),
                    it->name
                );
            }
            --depth;

            print_line(depth, "}");
        } break;

        case Decl_Enum: {
            print_line(depth, "enum %s (0x%llX) {\n", type->name.s, (umm)type);

            ++depth;
            ForArray (&type->members) {
                print_line(depth, "%s,\n", it->name);
            }
            --depth;

            print_line(depth, "}");
        } break;

        case Decl_TypeDef: {
            print_line(depth, "typedef %s %s;", type->typedef_src->name.s, type->name.s);
        } break;

        case Decl_Stub: {
            print_line(depth, "%s", type->name);
        } break;

        default: {
            /* ... */
        } break;
    }
}

}

//
// NOTE: Main
//

int main(int argc, char** argv) {
    u32 file_size;
    char* file_data;
    if (read_entire_file_null_terminated("..\\..\\rogue\\src\\rogue_platform.h", &file_size, &file_data)) {
        Parser parser;
        init_parser(&parser, "rogue_platform.h", file_data);
        // init_parser(&parser, "input", input);
        // init_parser(&parser, "alt_input", alt_input);

        while (peek_token(&parser) != CLEX_eof) {
            if (peek_token(&parser) == CLEX_parse_error) {
                error_at_token(&parser, "Internal parser error. Sorry!");
            }

            char* parse_point = parser.lex.parse_point;

            int error_code = setjmp(parser.error_jmp);
            if (error_code) {
                // NOTE: Throw away most recent the top level decl, cuz it errored out, and is therefore garbage
                Decl* freed_decl = remove_last_top_level_decl_and_free_children(&parser);
                if (freed_decl) {
                    warn(&parser, "Top-level declaration %s and sub-declarations have been thrown out, because there was a parsing error.", freed_decl->name.s);
                }
                // NOTE: And reset to the global namespace
                // TODO: We could probably just ensure when parsing top level declarations we're always
                //       back in the global namespace, not sure if that's better.
                parser.first_namespace = &parser.global_namespace;
            } else {
                parse_type(&parser);
            }

            parser.top_level_decl_being_parsed = 0;

            if (parser.lex.parse_point == parse_point) {
                next_token(&parser);
            }
        }

        printf("\n");
        printf("-------------\n");
        printf("Unknown types\n");
        printf("-------------\n\n");

        for (u32 decl_index = 0; decl_index < parser.global_namespace.decls.count; ++decl_index) {
            Decl* decl = parser.global_namespace.decls[decl_index];
            if (decl && decl->kind == Decl_Stub) {
                print_type(decl);
                printf(" (0x%llX)\n", (umm)decl);
            }
        }

        printf("\n");
        printf("-----------\n");
        printf("Known types\n");
        printf("-----------\n\n");

        for (u32 decl_index = 0; decl_index < parser.global_namespace.decls.count; ++decl_index) {
            Decl* decl = parser.global_namespace.decls[decl_index];
            if (decl && decl->kind != Decl_Stub) {
                print_type(decl);
                printf("\n\n");
            }
        }
    } else {
        fprintf(stderr, "Could not open file :(\n");
    }
}