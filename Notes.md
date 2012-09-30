
## Modulo

- Generates modular C99 headers from simple definitions
  - The original definitions parsed back from the generated C files
  - Optionally generates FFI wrappers for Lisp, Haskell etc


### Primitive ypes
    Char Short Int Long LongLong
    UChar UShort UInt ULong ULongLong
    Float Double LongDouble
    Int8 Int16 Int32 Int64 UInt8 UInt16 UInt32 UInt64
    Void Size Ptrdiff Intptr UIntptr
    
### Type constructors
    Pointer       a*
    Array         [a x N]
    Function      (a, b) -> c
    Struct        struct { a: A, b: B }
    Union         union { a: A, b: B }
    Tagged union  data { a: A, b: B }
    Enum          enum { a, b, c }
    Alias         b 
    


### Generic FFI support

C side
------
- Call client
    - Passing simple values by value
    - Passing compound values by reference
- Managing memory
    - Allocating simple values on stack and heap
    - Deallocate heap values
- Read and write to memory
    - Simple values
    - Compound values

L side (where L is any language with a C FFI)
------
- Call C
    - Passing simple types by value
    - Passing compound types by reference
- Managing memory
    - Allocating simple values on stack and heap
    - Deallocate heap values
- Read and write to memory
    - Simple values





