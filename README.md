
## Modulo

- Generates modular C99 headers from simple definitions
- The original definitions parsed back from the generated C files
- Optionally generates FFI wrappers for Lisp, Haskell etc


Types
    Char Short Int Long LongLong
    UnsignedChar UnsignedShort UnsignedInt UnsignedLong UnsignedLongLong
    Float Double LongDouble
    Int8 Int16 Int32 Int64 UInt8 UInt16 UInt32 UInt64
    Void Size Ptrdiff Intptr UnsignedIntptr
    
Conventional types
    AsciiStr = ptr Char
    Utf8Str  = ptr Char
    Utf16Str = ptr UInt16
    Utf32Str = ptr UInt32
    
Type constructors
    Pointer     ptr a
    DynArray    [a]
    Array       [a x N]
    Function    a, b -> c
    Struct      struct { a : A, b : B }
    Union       union { a : A, b : B }
    TagUnion    data { a : A, b : B }   ==> struct { enum { A_, B_ }, union { A, B } }
    Enum        enum { a, b, c }
    Alias       type a = b 
    

Modules
Constants
"Methods"

    module Foo {
        module Bar {
            import X.Y.Z.Baz;
            
            struct Note { Pitch, On, Off };
            enum Pitch { C, D, E, F, G };
            
            foo = 5 : Int;

            foo : Note -> Note;
            bar : Pitch -> Pitch;
        }
    }
            
    ==>
        #ifndef _FOO_BAR_BAZ
        #define _FOO_BAR_BAZ

        #include <x/y/z/note.h>
    
        struct _foo_bar_note_t { ... };
        typedef struct _foo_bar_note_t foo_bar_note_t;
    
        enum _foo_bar_pitch { c, d, e, f g, };
        typedef enum _foo_bar_pitch foo_bar_pitch;
    
        static const int foo_bar_foo = 5;

        note_t foo_bar_foo(note_t);
        pitch_t foo_bar_bar(pitch_t);
        
        #endif // _FOO_BAR_BAZ
    
    ==>
        (using-package :foo-bar)  
    
        (defcstruct foo_bar_note_t)
        (defcenum foo_bar_pitch_t :c :d :e :f :g)
        (defcvar foo_bar_foo :int)
        ;(defcfun foo_bar_foo :note_t)
    
    
=============

Generic FFI support

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





