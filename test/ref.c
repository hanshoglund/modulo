
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> // for memcpy

typedef T* ptr;
typedef void(deleter*)(T*);
typedef struct { ptr p, deleter d } unique_ptr;
typedef struct { ptr p, deleter d } shared_ptr;

unique_ptr new_unique_ptr(ptr, deleter);      // p = new(x,d)  then p contains x
void       delete_unique_ptr(unique_ptr);     // delete(p)     then p contains nothing
unique_ptr move_unique_ptr(unique_ptr);       // q = move(p)   then y contains x and p nothing
void swap_unique_ptr(unique_ptr,unique_ptr);  // swap(p,q)     then p contains y and q contains x  
ptr release_unique_ptr(ptr);                  // release(x)    then p contains y
ptr unsafe_unwrap_unique_ptr();


shared_ptr new_shared_ptr(ptr, deleter);
void       delete_shared_ptr(shared_ptr);
shared_ptr copy_shared_ptr(shared_ptr p);
shared_ptr swap_shared_ptr(shared_ptr p);
ptr release_shared_ptr(ptr);
ptr unsafe_unwrap_shared_ptr();

// TODO weak from shared
