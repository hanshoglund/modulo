
# Modulo

Modulo is a tool that generates modular, standard C interfaces based on a high-level 
description language. The idea is to specify functionality in the module language 
and implement it in any language that supports C-style calling conventions. 

This package include generators for C, JavaScript, Haskell and Common Lisp.

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)
* [Prettify](https://github.com/hanshoglund/prettify)

## Installation

    cabal configure
    cabal install

## Usage

    Usage: modulo [options]
    Usage: modulo [options] files...

    Languages:
      C
      Lisp
      JavaScript
      Haskell

    Options:
      -h        --help                Print help and exit
      -v        --version             Print version and exit
      -L[LANG]  --language[=LANG]     Output language
      -M[PATH]  --module-path[=PATH]  Module paths

    
### Module language 

### Overview

    Pointer       a*
    Array         [a x N]
    Function      (a, b) -> c
    Struct        struct { a: A, b: B }
    Union         union { a: A, b: B }
    Tagged union  data { a: A, b: B }
    Enum          enum { a, b, c }
    Alias         b 

#### Description

The module language is very simple. Each module consists of a name followed by
eventual import declarations, followed by all other declarations. Here is an example module:

    module List
    {
    opaque Elem;
    opaque List;

    nil     : () -> List;
    cons    : (Elem, List) -> List;
    head    : (List) -> Elem;
    tail    : (List) -> List;

    empty   : (List) -> Bool;
    lenght  : (List) -> Int;

    reverse : (List) -> List;
    sort    : (List) -> List;
    }

Like C, the module language uses structural typing for pointers and functions, but
nominal typing for structures and unions. Thus in the following example values of type
`A` and `B` are interchangeable, but values of type `C` and `D` are not.

    type A = Ptr Int
    type B = Ptr Int
    type C = struct { x : Int, y : Int }
    type D = struct { x : Int, y : Int }

The pointer type constructor can be used with any type:

    type IntPtr = Int*
    type FloatPtr = Float*

The array type constructor need an explicit length:

    type IntVec = [Int x 10]

Functions are written as in Haskell, except for the parentheses enclosing the arguments.
Thus, higher-arity functions are easily distinguished.

    type NoCurry = (A, B) -> C
    type Curry   = (A) -> (B) -> C

Compound types are written in a uniform manner:

    type IntCounter = struct { a : Int, b : Int -> Int }
    type NumValue   = union { left : Int, right : Float }
    type Color      = enum { Red, Green, Blue }

The following primitive types are provided:

    Void Size Ptrdiff Intptr UIntptr 
    Char Short Int Long LongLong
    UChar UShort UInt ULong ULongLong
    Float Double LongDouble
    Int8 Int16 Int32 Int64 UInt8 UInt16 UInt32 UInt64
