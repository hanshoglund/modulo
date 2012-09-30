
#include <stdio.h>

// typedef struct _foo { int x; float y; } foo;
typedef struct _foo {
            int x; int y;
        } foo;

typedef struct _foo2 { int x; float y; } foo2;
typedef union _bar { int x; float y; } bar;
typedef union _bar2 { int x; float y; } bar2;
typedef enum _baz { x, y, z } baz;
typedef enum { xx, yy, zz } baz2;

void f(int x)
{    
}
void g(int x[])
{    
}

typedef int (*int_ternary_t)(int, int, int);

typedef int int_t;
typedef int int_ar_t[10];
typedef struct { int_ternary_t x; int_ternary_t y; } ternary_pair_t;

int_ternary_t my_ternary;

typedef struct {} fox;



int my_vector[] = { 1,2,3 };


// Forward declare mixup
typedef union _foreword foreword;
void make_book(foreword* fw);


int main (int argc, char const *argv[])
{         
    foo x;
    foo2 y;

    bar a;
    bar2 b;
    union _bar2 c;

    baz m;
    baz2 n;    

    int aa[10] = {};
    f(*aa); f(aa[0]);
    g(aa);

    int xx;
    f(xx);
    g(&xx);
        
    printf("Hello!\n");
    return 0;
}   

                
// a[0] T         x  T          *P  T  
// a    T[]       &x T*          p  T*
