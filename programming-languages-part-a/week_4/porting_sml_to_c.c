#include <stdlib.h>
#include <stdbool.h>

// Function pointers are only code pointers
// HO functions will take explicit environment fields as argument
// void* requires unchecked conversions between types, no notion of type var

typedef struct List list_t;
struct List {
    void *head;
    list_t *tail;
};

list_t *make_list (void *x, list_t *xs) {
    list_t *ans = (list_t *)malloc(sizeof(list_t));
    ans->head = x;
    ans->tail = xs;
    return ans;
}

list_t *map(void* (*f)(void*, void*), void* env, list_t *xs) {
    if (xs == NULL)
        return NULL;
    return make_list(f(env, xs->head), map(f, env, xs->tail));
}

list_t *filter(bool (*f)(void*, void*), void* env, list_t *xs) {
    if (xs == NULL)
        return NULL;
    if (f(env, xs->head))
        return make_list(xs->head, filter(f, env, xs->tail));
    return filter(f, env, xs->tail);
}

int length(list_t *xs) {
    int ans = 0;
    while (xs!=NULL) {
        ++ans;
        xs = xs->tail;
    }
    return ans;
}

// awful type casts to match what map expects
void* doubleInt(void* ignore, void* i) {
    return (void*)(((__intptr_t)i) * 2);
}

// assumes list holds intptr_t fields
list_t *doubleAll(list_t *xs) {
    return map(doubleInt, NULL, xs);
}

// awful type casts to match what filter expects
bool isN(void* n, void* i) {
    return ((__intptr_t)i) == ((__intptr_t)n);
}

// assumes list hold intptr fields
int countNs(list_t *xs, __intptr_t n) {
    return length(filter(isN, (void*)n, xs));
}
