//
// Created by Berci on 2021. 02. 18..
//

#ifndef WAMC_WAM_H
#define WAMC_WAM_H

#include <stdbool.h>
#include <stddef.h>

/*
 * TYPES
 */

// cell types
typedef enum {
    STR,    // structure reference, ie | STR | 1 |
    REF,    // variable reference, ie | REF | 3 |

    CE,     // continuation environment pointer
    N,      // number of permanent variables in corresponding stack frame

    FUNC,   // functor description, ie `h/2`
} Tag;

// read/write mode
typedef enum {
    READ,
    WRITE
} Mode;

typedef struct {
    const char* name;
    size_t arity;
} Structure;

bool structure_eq(Structure f1, Structure f2);

typedef struct {
    Tag tag;
    union {
        size_t address;         // used by: REF, STR, CE, N
        Structure structure;    // used by: FUNC
    };
} Cell;

/*
 * WAM INSTRINSICS
 */

size_t deref(size_t address);
void bind(size_t a, size_t b);
void unify(size_t a, size_t b);

/*
 * REGISTER ACCESS
 */

size_t X(size_t ptr);
size_t A(size_t ptr);

/*
 * STACK ACCESS
 */
size_t STACK(size_t ptr);

/*
 * PDL MANIPULATION
 */

void pdl_push(size_t a);
size_t pdl_pop();
void pdl_clear();
bool pdl_empty();

/*
 * WAM INSTRUCTIONS
 */

void put_structure(Structure functor, size_t ptr);
void get_structure(Structure structure, size_t ptr);

void set_variable(size_t ptr);
void unify_variable(size_t ptr);
void put_variable(size_t ptrX, size_t ptrA);
void get_variable(size_t ptrX, size_t ptrA);

void set_value(size_t ptr);
void unify_value(size_t ptr);
void put_value(size_t ptrX, size_t ptrA);
void get_value(size_t ptrX, size_t ptrA);

void allocate(size_t n);
void deallocate(size_t n);

/*
 * RESULT
 */

void report(const char* var, size_t ptr);

#endif //WAMC_WAM_H
