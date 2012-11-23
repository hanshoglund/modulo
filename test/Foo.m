
/**
    @defgroup Bar
    This is a test module.
 */
module Foo.Bar
{
  import Std;
  import List;

  // Type declarations

  type Foo = Int;
  type Bar = [Int* x 10];
  type Baz = [Int x 10]*;
  type Bat = (Int, [Int x 2]) -> [Int x 10]*;
  type Nest = struct { 
    x: [[struct { y : Int } x 10] x 20]
  };
                    
  type Ptr = (Int) -> Int*;

  type Curried = (Int) -> Int;
  type Curry = (Int) -> Curried*;
  type Hask  = ((Int) -> Int) -> Int;
  type HaskPtr = Hask*;

  // type IntCounter = struct { a : Int, b : (Int) -> Int };

  /**
      @name  This is the name.
      @param These are the parameters.
   */
  type T = Foo;                               // an alias is written as its name
  type A = Int;                               // same goes for the primitive types
  type B = Int*;                              // pointer
  type C = [Int x 10];                        // array
  type D = (Int, Int) -> Int;                 // function
  type E = enum { Sweden, Norway };           // anonymous enum
  type F = union { x: Int, y: Int };          // anonymous union
  type G = struct { x: Int, y: HaskPtr };     // anonymous struct

  type Prims = struct {
      v : Void*,
      i : Int8,
      j : Int16,
      k : Int32
  };
  
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
  
  fromVoid : () -> Int;
  toVoid : (Int) -> Void;
  // compose : ((B) -> C) -> ((A) -> B) -> (A) -> C;
}




