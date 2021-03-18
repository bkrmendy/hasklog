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

typedef struct Cell {
    Tag tag;
    union {
        struct Cell* ptr;         // used by: REF, STR, CE, N
        Structure structure;    // used by: FUNC
    };
} Cell;

/*
 * WAM INSTRINSICS
 */

Cell* deref(Cell* cell);
void bind(Cell* a, Cell* b);
void unify(Cell* a, Cell* b);

/*
 * REGISTER ACCESS
 * Translates register number to store pointer
 */

Cell* X(size_t ptr);

/*
 * PDL MANIPULATION
 */

void pdl_push(Cell* cell);
Cell* pdl_pop();
void pdl_clear();
bool pdl_empty();

/*
 * WAM INSTRUCTIONS
 */

void put_structure(Structure functor, Cell* cell);
void get_structure(Structure structure, Cell* cell);

void set_variable(Cell* cell);
void unify_variable(Cell* cell);
void put_variable(Cell* x, Cell* a);
void get_variable(Cell* x, Cell* a);

void set_value(Cell* cell);
void unify_value(Cell* cell);
void put_value(Cell* x, Cell* a);
void get_value(Cell* x, Cell* a);

void print_heap(size_t depth);
void print_xs();

void tryMeElse();
void retryMeElse();
void trustMe();
/*
 * RESULT
 */

void report(const char* var, Cell* cell);

#endif //WAMC_WAM_H
