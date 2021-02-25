#include "wam.h"

void p_3() {
    {
        Structure f = { .name = "f", .arity = 1 };
        get_structure(f, X(1));
    }
    unify_variable(X(4));
    {
        Structure h = { .name = "h", .arity = 2 };
        get_structure(h, X(2));
    }
    unify_variable(X(5));
    unify_variable(X(6));
    get_value(X(5), X(3));
    {
        Structure f = { .name = "f", .arity = 1 };
        get_structure(f, X(6));
    }
    unify_variable(X(7));
    {
        Structure a = {.name = "a", .arity = 0};
        get_structure(a, X(7));
    }
}

/*
 * QUERY
 */
void query() {
    put_variable(X(4), X(1));
    {
        Structure h = { .name = "h", .arity = 2 };
        put_structure(h, X(2));
    }
    set_value(X(4));
    set_variable(X(5));
    {
        Structure f = { .name = "f", .arity = 1 };
        put_structure(f, X(3));
    }
    set_value(X(5));
    p_3();
}

int main() {
    query();
    report("Z", X(4));
    report("W", X(5));
    return 0;
}
