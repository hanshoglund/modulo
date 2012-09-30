
module Foo {
    // import X.Y.Z.Baz;
    // import Fluid;

    type Foo = Int;
    type Bar = [Int* x 10];


    // Foo             // an alias is written as its name
    // Int             // same goes for the primitive types
    // Int*            // pointer
    // [A x 10]        // array
    // (A, B) -> C     // function
    // enum { a, b, c}                 // anonymous enum
    // union { x:Int, y:Int }          // anonymous union
    // struct { x:Int, y:Int }         // anonymous struct
    // bitfield { x:Int8, y:Int16 }    // bitfield (length calculated automagically)
    // struct "Foo"                    // struct/union/enum with the given tag
    // 
    // // Tag declaration
    // tag struct "_pair" { /* ...*/  };           // ==> struct _pair { /* ... */ };
    // 
    // // Type declarations
    // type Foo = Int;                              // ==> typedef int foo;
    // type Pair = struct { x:Int, y:Int };         // ==> typedef struct { int x; int y; } pair;
    // type Pair = struct "_Pair" { x:Int, y:Int }; // ==> typedef struct _pair { int x; int y; } pair;
    // 
    // // Forward declaration (just structs for now)
    // type Foo;
    // 
    // 
    // 
    // 
    // type Note = struct { Pitch, On, Off }
    // type Note = struct { Pitch, On, Off }
    // type Pitch = enum { C, D, E, F, G }
    // type Foo = union { x : Pitch, y : Note }
    // 
    // plus : (x:Int, y:Int) -> Int
    // 
    // 
    // // 
    // // struct Note { Pitch, On, Off };
    // // enum Pitch { C, D, E, F, G };
    // // 
    // // foo = 5 : Int;
    // // 
    // // foo : Note -> Note;
    // // bar : Pitch -> Pitch;  
}
