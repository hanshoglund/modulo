
## Modulo

- Generates modular C99 headers from simple definitions
- The original definitions parsed back from the generated C files
    - This may require some "smart" comments in the generated files
- Optionally generates FFI wrappers for Lisp, Haskell etc


Types
    Char Short Int Long LongLong
    UnsignedChar UnsignedShort UnsignedInt UnsignedLong UnsignedLongLong
    Void Float Double LongDouble
    Int8 Int16 Int32 Int64 UInt8 UInt16 UInt32 UInt64
    
Conventional types
    AsciiStr = Ptr Char
    Utf8Str  = Ptr Char
    Utf16Str = Ptr UInt16
    Utf32Str = Ptr UInt32
    
Type constructors
    Pointer     Ptr a
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

    module Foo
        module Bar
            import X.Y.Z.Baz
            
            struct Note { Pitch, On, Off }
                default : Note
            
            enum Pitch { C, D, E, F, G }
            
            foo = 5 : Int

            foo : Note -> Note
            bar : Pitch -> Pitch
            
            
    ==>
        #ifndef _FOO_BAR_BAZ
        #define _FOO_BAR_BAZ

        #include <x/y/z/note.h>
    
        /* Note type */
        struct _foo_bar_note_t { ... };
        typedef struct _foo_bar_note_t foo_bar_note_t;
        note_t foo_bar_note_default(note_t);
    
        /* Pitch type */
        typedef enum foo_bar_pitch { c, d, e, f g, };
    
        static const int foo_bar_foo = 5;

        note_t foo_bar_foo(note_t);
        pitch_t foo_bar_bar(pitch_t)
        
        #endif // _FOO_BAR_BAZ
    
    ==>
        (using-package :foo-bar)  
    
        (defcstruct foo_bar_note_t)
        (defcenum foo_bar_pitch_t :c :d :e :f :g)
        (defcvar foo_bar_foo :int)
        ;(defcfun foo_bar_foo :note_t)
    
    
