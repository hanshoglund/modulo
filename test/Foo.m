
module Doremir.Util.List
{
  import Doremir.Util.Ref;

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
  
  // TODO
  // type H = bitfield { x:Int8, y:Int16 };   // bitfield (length calculated automagically)

  // TODO
  // type I = struct "Foo";                   // struct/union/enum with the given tag

  // TODO
  // Tag declaration
  // tagname struct "name" { x:Int };

  // TODO
  // Forward declaration (just structs for now)
  // type Foo;

  // Function declarations
  plus : (Int, Int) -> Int;
  fib : (Int) -> Int;

  // TODO
  // Constant declarations
  // const pi = 3.1415 : Int;
  // var errNo : Int;
  
  // fromVoid : () -> Int;
  // toVoid : Int -> ();
  compose : ((B) -> C) -> ((A) -> B) -> (A) -> C;
}




