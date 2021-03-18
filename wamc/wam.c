//
// Created by Berci on 2021. 02. 18..
//

#include "wam.h"

#include <string.h>
#include <assert.h>
#include <stdio.h>

bool structure_eq(Structure f1, Structure f2) {
    return strcmp(f1.name, f2.name) == 0 && f1.arity == f2.arity;
}

void cell_assign(Cell* a, Cell* b) {
    a->tag = b->tag;
    a->ptr = b->ptr;
    a->structure = b->structure;
}

/*
 * GLOBALS
 */

Cell HEAP[1000];       // general memory
Cell* H = HEAP;         // heap pointer
Cell XS[10];           // Registers
Cell* S = HEAP;           // ???
bool fail = false;      // fail bit
Mode mode = READ;       // mode

Cell* PDL[1000];       // push_down list
int PDL_PTR = -1;       // empty stack

Cell* X(size_t ptr) {    // calculate address of Xi register
    return &XS[ptr];
}

void pdl_push(Cell* cell) {
    assert(PDL_PTR < 999 && "Stack overflow!");
    PDL[PDL_PTR + 1] = cell;
    PDL_PTR += 1;
}

Cell* pdl_pop() {
    assert(PDL_PTR >= 0 && "Stack underflow!");
    Cell* cell = PDL[PDL_PTR];
    PDL_PTR -= 1;
    return cell;
}

bool pdl_empty() {
    return PDL_PTR < 0;
}

void pdl_clear() {
    PDL_PTR = -1;
}

/*
 * AUXILIARY FUNCTIONS
 */

Cell* deref(Cell* cell) {
    while (cell->tag == REF && cell->ptr != cell) {
        cell = cell->ptr;
    }
    return cell;
}

void bind(Cell* a, Cell* b) {
    a->tag = REF;
    a->ptr = b;
}

void unify(Cell* a, Cell* b) {
    pdl_clear();
    pdl_push(a); pdl_push(b);
    fail |= false;
    while (!(pdl_empty() || fail)) {
        Cell* d1 = deref(pdl_pop());
        Cell* d2 = deref(pdl_pop());

        if (d1 == d2) {
            continue;
        }

        if (d1->tag == REF && d1->ptr == d1) {
            bind(d2, d1);
        } else if (d2->tag == REF && d2->ptr == d2) {
            bind(d1, d2);
        } else {
            Cell* f1 = d1->ptr;
            Cell* f2 = d2->ptr;
            assert(f1->tag == FUNC);
            assert(f2->tag == FUNC);
            if (structure_eq(f1->structure, f2->structure)) {
                for (size_t iarg = 1; iarg <= f1->structure.arity; iarg++) {
                    pdl_push(f1 + iarg);
                    pdl_push(f2 + iarg);
                }
            } else {
                fail = true;
            }
        }
    }
}

void reportI(Cell* cell) {
    assert(cell != cell->ptr);
    Cell* funcCell = deref(cell)->ptr;
    assert(funcCell->tag == FUNC);
    printf("%s", funcCell->structure.name);

    if (funcCell->structure.arity > 0) {
        printf("(");
        for (size_t iarg = 0; iarg < funcCell->structure.arity; ++iarg) {
            reportI(funcCell + 1 + iarg);
            if (iarg < funcCell->structure.arity - 1) {
                printf(", ");
            }
        }
        printf(")");
    }
}

void report(const char* var, Cell* cell) {
    if (fail) {
        printf("fail!");
        return;
    }
    printf("%s = ", var);
    reportI(cell);
    printf("\n");
}

void print_cell(Cell* cell) {
    if (cell->tag == REF) {
        printf("%p | REF | %p\n", cell, cell->ptr);
    } else if (cell->tag == FUNC) {
        printf("%p | %s/%zu\n", cell, cell->structure.name, cell->structure.arity);
    } else if (cell->tag == STR) {
        printf("%p | STR | %p\n", cell, cell->ptr);
    } else {
        assert(0 && "Unknown tag");
    }
}

void print_heap(size_t depth) {
    Cell* HBase = HEAP;
    for (size_t d = 0; d < depth; d++) {
        print_cell(HBase);
        HBase += 1;
    }
}

void print_xs() {
    printf("XS:\n");
    for (size_t i = 0; i < 10; i++) {
        print_cell(X(i));
    }
    printf("----------------------\n");
}

/*
 * L1 (according to wambook)
 */

void put_structure(Structure functor, Cell* cell) {
    H->tag = STR;
    H->ptr = H + 1;
    (H + 1)->tag = FUNC;
    (H + 1)->structure = functor;
    cell_assign(cell, H);
    H = H + 2;
}

void set_variable(Cell* cell) {
    H->tag = REF;
    H->ptr = H;
    cell_assign(cell, H);
    H = H + 1;
}

void set_value(Cell* cell) {
    cell_assign(H, cell);
    H = H + 1;
}

void get_structure(Structure structure, Cell* cell) {
    Cell* addr = deref(cell);
    if (addr->tag == REF) {
        H->tag = STR;
        H->ptr = H + 1;
        (H + 1)->tag = FUNC;
        (H + 1)->structure = structure;
        bind(addr, H);
        H = H + 2;
        mode = WRITE;
    } else if (addr->tag == STR) {
        Cell *ic = addr->ptr;
        assert(ic->tag == FUNC);
        if (structure_eq(ic->structure, structure)) {
            S = addr->ptr + 1;
            mode = READ;
        } else {
            fail = true;
        }
    } else {
        assert(0 && "FUNC cannot be here");
    }
}

void unify_variable(Cell* cell) {
    if (mode == READ) {
        cell_assign(cell, S);
    } else if (mode == WRITE) {
        H->tag = REF;
        H->ptr = H;
        cell_assign(cell, H);
        H = H + 1;
    }
    S = S + 1;
}

void unify_value(Cell* cell) {
    if (mode == READ) {
        unify(cell, S);
    } else if (mode == WRITE) {
        cell_assign(H, cell);
        H = H + 1;
    }
    S = S + 1;
}

void put_variable(Cell* x, Cell* a) {
    H->tag = REF;
    H->ptr = H;
    cell_assign(x, H);
    cell_assign(a, H);
    H = H + 1;
}

void put_value(Cell* x, Cell* a) {
    cell_assign(a, x);
}

void get_variable(Cell* x, Cell* a) {
    cell_assign(x, a);
}

void get_value(Cell* x, Cell* a) {
    unify(x, a);
}