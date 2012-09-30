
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> // for memcpy

typedef T val;                  

typedef struct { val(*f)(val);         int n;  val as[1];  } f1;
typedef struct { val(*f)(val,val);     int n;  val as[2];  } f2;
typedef struct { val(*f)(val,val,val); int n;  val as[3];  } f3;

val ap1(f1, val);
f1  ap2(f2, val);
f2  ap3(f3, val);