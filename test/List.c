
#ifndef _DATA_LIST
#define _DATA_LIST

#include <stdbool.h>

typedef void * Elem;
typedef void * Filter;
typedef struct {
            int x;
        } ListT;
typedef struct {
            Elem head; ListT tail;
        } * List;
List nil();
List cons(Elem, List);
Elem head(List);
List tail(List);
List init(List);
Elem last(List);
bool empty(List);
int lenght(List);
List reverse(List);
List sort(List);
List reverse(List);
List intersperse(Elem, List);
List transpose(List);
List subsequences(List);
List permutations(List);
List concat(List);
bool and(List);
bool or(List);
bool any(Filter, List);
bool all(Filter, List);

#endif // _DATA_LIST

