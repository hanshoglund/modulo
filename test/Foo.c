
#ifndef _DOREMIR_AUDIO_DISPATCHER
#define _DOREMIR_AUDIO_DISPATCHER

#include <stdint.h>

typedef int Foo;
typedef int * Bar[10];
typedef int (* Baz)[10];
typedef int (* Bat(int, int [2]))[10];
typedef struct {
            struct {
                int y;
            } x[20][10];
        } Nest;
typedef int * Ptr(int);
typedef int Curried(int);
typedef Curried * Curry(int);
typedef int Hask(int (int));
typedef Hask * HaskPtr;
typedef Foo T;
typedef int A;
typedef int * B;
typedef int C[10];
typedef int D(int, int);
typedef enum {
            Sweden, Norway
        } E;
typedef union {
            int x; int y;
        } F;
typedef struct {
            int x; HaskPtr y;
        } G;
typedef struct {
            void * v; int8_t i; int16_t j; int32_t k;
        } Prims;
int plus(int, int);
int fib(int);
int fromVoid();
void toVoid(int);

#endif // _DOREMIR_AUDIO_DISPATCHER

