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

Cell STORE[1000];       // general memory
size_t X_base = 0;      // pointer to start of Xs
size_t A_base = 50;     // pointer to start of As
size_t H = 100;         // heap pointer
size_t S = 0;           // ???
size_t E = 500;         // stack pointer
bool fail = false;      // fail bit
Mode mode = READ;       // mode

size_t PDL[1000];       // push_down list
int PDL_PTR = -1;       // empty stack

size_t deref(size_t address) {
    Cell c = STORE[address];
    while (c.tag == REF && c.address != address) {
        address = c.address;
        c = STORE[address];
    }
    return address;
}

void bind(size_t a, size_t b) {
    Cell refCell = {.tag = REF, .address = b }; STORE[a] = refCell;
}

void unify(size_t a, size_t b) {
    pdl_clear();
    pdl_push(a); pdl_push(b);
    fail |= false;
    while (!(pdl_empty() || fail)) {
        size_t d1 = deref(pdl_pop());
        size_t d2 = deref(pdl_pop());

        if (d1 == d2) {
            continue;
        }

        Cell c1 = STORE[d1]; // { t1, v1 }
        Cell c2 = STORE[d2]; // { t2, v2 }

        if (c1.tag == REF && c1.address == d1) {
            bind(d1, d2);
        } else if (c2.tag == REF && c2.address == d2) {
            bind(d2, d1);
        } else {
            Cell f1 = STORE[c1.address];
            Cell f2 = STORE[c2.address];
            if (f1.tag == FUNC && f2.tag == FUNC && structure_eq(f1.structure, f2.structure)) {
                for (size_t iarg = 1; iarg <= c1.structure.arity; iarg++) {
                    pdl_push(c1.address + iarg);
                    pdl_push(c2.address + iarg);
                }
            } else {
                fail = true;
            }
        }
    }
}

size_t X(size_t ptr) {    // calculate address of Xi register
    return X_base + ptr;
}

size_t A(size_t ptr) {    // calculate address of Ai register
    return A_base + ptr;
}

size_t STACK(size_t ptr) {
    return E + ptr;
}

void pdl_push(size_t a) {
    assert(PDL_PTR < 999 && "Stack overflow!");
    PDL[PDL_PTR + 1] = a;
    PDL_PTR += 1;
}

size_t pdl_pop() {
    assert(PDL_PTR >= 0 && "Stack underflow!");
    size_t value = PDL[PDL_PTR];
    PDL_PTR -= 1;
    return value;
}

bool pdl_empty() {
    return PDL_PTR < 0;
}

void pdl_clear() {
    PDL_PTR = -1;
}

void put_structure(Structure functor, size_t ptr) {
    Cell strCell = { .tag = STR, .address = H + 1 }; STORE[H] = strCell;
    Cell funcCell = { .tag = FUNC, .structure = functor }; STORE[H + 1] = funcCell;
    STORE[ptr] = STORE[H];
    H = H + 2;
}

void set_variable(size_t ptr) {
    Cell nuCell = { .tag = REF, .address = H }; STORE[H] = nuCell;
    STORE[ptr] = STORE[H];
    H = H + 1;
}

void set_value(size_t ptr) {
    STORE[H] = STORE[ptr];
    H = H + 1;
}

void get_structure(Structure structure, size_t ptr) {
    size_t addr = deref(ptr);
    Cell c = STORE[addr];
    if (c.tag == REF) {
        Cell strCell = { .tag = STR, .address = H + 1 }; STORE[H] = strCell;
        Cell funcCell = { .tag = FUNC, .structure = structure }; STORE[H + 1] = funcCell;
        bind(addr, H);
        H = H + 2;
        mode = WRITE;
        return;
    } else if (c.tag == STR) {
        Cell ic = STORE[c.address];
        if (ic.tag == FUNC && structure_eq(ic.structure, structure)) {
            S = c.address + 1;
            mode = READ;
            return;
        } else {
            fail = true;
        }
    } else {
        fail = true;
    }
}

void unify_variable(size_t ptr) {
    if (mode == READ) {
        STORE[ptr] = STORE[S];
    } else if (mode == WRITE) {
        Cell refCell = { REF, H }; STORE[H] = refCell;
        STORE[ptr] = STORE[H];
        H = H + 1;
    }
    S = S + 1;
}

void unify_value(size_t ptr) {
    if (mode == READ) {
        unify(ptr, S);
    } else if (mode == WRITE) {
        STORE[H] = STORE[ptr];
        H = H + 1;
    }
    S = S + 1;
}

void put_variable(size_t ptrX, size_t ptrA) {
    Cell refCell = { .tag = REF, .address = H }; STORE[H] = refCell;
    STORE[ptrX] = STORE[H];
    STORE[ptrA] = STORE[H];
    H = H + 1;
}

void put_value(size_t ptrX, size_t ptrA) {
    STORE[ptrA] = STORE[ptrX];
}

void get_variable(size_t ptrX, size_t ptrA) {
    STORE[ptrX] = STORE[ptrA];
}

void get_value(size_t ptrX, size_t ptrA) {
    unify(ptrX, ptrA);
}

void reportRec(size_t ptr) {
    size_t dptr = deref(ptr);
    Cell cell = STORE[dptr];

    if (cell.tag == STR) {
        Cell funcCell = STORE[dptr + 1];
        printf("%s", funcCell.structure.name);

        if (funcCell.structure.arity > 0) {
            printf("(");
            for (size_t iarg = 0; iarg < funcCell.structure.arity; ++iarg) {
                reportRec(dptr + 2 + iarg);
            }
            printf(")");
        }
    }
}

void report(const char* var, size_t ptr) {
    printf("%s = ", var);
    reportRec(ptr);
    printf("\n");
}

void allocate(size_t n) {
    size_t newE = E + STACK()
}
