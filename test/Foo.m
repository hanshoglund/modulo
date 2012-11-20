
module Foo.Bar.Baz
{
  // import X.Y.Z.Baz;
  // import List;
  // import List;

  // Type declarations

  type Foo = Int;
  type Bar = [Int* x 10];
  type Nest = struct { 
    x: [[struct { x: Int } x 10] x 20]
  };

  type T = Foo;                               // an alias is written as its name
  type A = Int;                               // same goes for the primitive types
  type B = Int*;                              // pointer
  type C = [Int x 10];                        // array
  type D = (Int, Int) -> Int;                 // function
  type E = enum { Sweden, Norway };           // anonymous enum
  type F = union { x: Int, y: Int };          // anonymous union
  type G = struct { x: Int, y: Int };         // anonymous struct
  // type H = bitfield { x:Int8, y:Int16 };   // bitfield (length calculated automagically)
  // type I = struct "Foo";                   // struct/union/enum with the given tag

  // Tag declaration
  // tagname struct "name" { x:Int };

  // Forward declaration (just structs for now)
  // type Foo;

  type Time = Double;
  type Pitch = enum { C, D, E, F, G };
  type Note = struct { pitch: Pitch, on: Time, off: Time };
  type PitchOrNote = union { x: Pitch, y: Note };

  plus : (Int, Int) -> Int;

  fib : (Int) -> Int;
                           
   
  // compose : ((B) -> C -> (A) -> B) -> (A) -> C;
}




