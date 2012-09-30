
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> // for memcpy

/* 
  Basic strategy:
    Two types for storing (unboxed) 64-bit values: list and flist
    For each list type, wrap in a "newtype struct" and do pointer casting

    Implement all functions that does not involve values on the list type (also convers flist)
    Implement value access thus:
      - Int types use list and assignment
      - Float types use flist and assignment
      - Struct types use list and memcpy
      
   Dangers:
    - Easy to mix list and flist in implementation! Leads to unintended int coersion.
 */


// Example of something that can be stored unboxed in a list (total 8 bytes)
struct _rgba { uint16_t r; uint16_t g; uint16_t b; uint16_t a; };
typedef struct _rgba rgba;

int compare_rgba(rgba x, rgba y)
{
  return 1
    && x.r == y.r
    && x.g == y.g
    && x.b == y.b
    && x.a == y.a;
}


// can store all prim pointers and ints by value
struct _list { struct _list* tail; int64_t head; };
typedef struct _list list;

// can store all prim floats by value (except long double)
struct _flist { struct _flist* tail; double head; };
typedef struct _flist flist;

// it is safe to cast:
//  list <--> flist



// can be safely casted to/from list (but not each other!)
typedef struct { list _; } list_short;
typedef struct { list _; } list_int;
typedef struct { list _; } list_long;
typedef struct { list _; } list_long_long;
typedef struct { list _; } list_float;
typedef struct { list _; } list_double;
typedef struct { list _; } list_rgba;

list_short* cons_short(short x, list_short* xs) 
{ 
  list* as = calloc(1, sizeof(list));
  as->head = x;
  as->tail = (list*) xs;
  return (list_short*) as;
}
list_int* cons_int(int x, list_int* xs) 
{ 
  list* as = calloc(1, sizeof(list));
  as->head = x;
  as->tail = (list*) xs;
  return (list_int*) as;
}
list_double* cons_double(double x, list_double* xs) 
{ 
  flist* as = calloc(1, sizeof(list));
  as->head = x;
  as->tail = (flist*) xs;
  return (list_double*) as;
}
list_rgba* cons_rgba(rgba x, list_rgba* xs) 
{                  
  list* as = calloc(1, sizeof(list));
  memcpy(&as->head, &x, sizeof(rgba));
  as->tail = (list*) xs;
  return (list_rgba*) as;
}

short       head_short(list_short* xs)          { return ((list*) xs)->head; }
int         head_int(list_int* xs)              { return ((list*) xs)->head; }
long        head_long(list_long* xs)            { return ((list*) xs)->head; }
long long   head_long_long(list_long_long* xs)  { return ((list*) xs)->head; }
float       head_float(list_float* xs)          { return ((flist*) xs)->head; }
double      head_double(list_double* xs)        { return ((flist*) xs)->head; }

rgba head_rgba(list_rgba* xs) 
{ 
  rgba r;
  memcpy(&r, &((list*) xs)->head, sizeof(rgba));
  return r;
}


void test()
{
  rgba red = { 65535, 0, 0, 0 };
  rgba green = { 0, 65535, 0, 0 };
  printf("red == red: %d\n", compare_rgba(red, red));
  printf("red == green: %d\n", compare_rgba(red, green));

  list_rgba* colors = NULL;
  colors = cons_rgba(red, colors);
  colors = cons_rgba(green, colors);

  rgba green2 = head_rgba(colors);
  printf("red == green2: %d\n", compare_rgba(red, green2));
  printf("green == green2: %d\n", compare_rgba(green, green2));
}


int main (int argc, char const *argv[])
{
  list_short* as;
  list_int* bs = cons_int(1, NULL);
  list_int* cs = cons_int(1, bs);
  
  printf("Size of double: %lu\n", sizeof(double));
  printf("Size of rgba: %lu\n", sizeof(rgba));
  printf("Size of list_short: %lu\n", sizeof(list_short));
  printf("Size of list_float: %lu\n", sizeof(list_short));
  
  test();
  
  return 0;
}