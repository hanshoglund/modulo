
module Data.List 
{
    import stdbool;
    
    type Elem   = Void*;
    type Filter = Void*;
    type ListT  = struct { x : Int }; // TODO get rid of
    type List = struct { 
        head : Elem, 
        tail : ListT }*;
            
    // type List = { nil : List* | head : List*, tail : T }

    nil     : () -> List;            //< The empty list
    cons    : (Elem, List) -> List;  //< The empty list
    head    : (List) -> Elem;        //< The empty list
    tail    : (List) -> List;
    init    : (List) -> List;
    last    : (List) -> Elem;

    empty   : (List) -> Bool;
    lenght  : (List) -> Int;

    reverse : (List) -> List;
    sort    : (List) -> List;

    reverse         : (List) -> List;
    intersperse     : (Elem, List) -> List;

    transpose       : (List) -> List;
    subsequences    : (List) -> List;
    permutations    : (List) -> List;
    concat          : (List) -> List;
    and             : (List) -> Bool;
    or              : (List) -> Bool;
    any             : (Filter, List) -> Bool;
    all             : (Filter, List) -> Bool;
    // sum             : Num a => List -> a
    // product         : Num a => List -> a
    // maximum         : Ord a => List -> a
    // minimum         : Ord a => List -> a
    // iterate         : (a -> a) -> a -> List
    // repeat          : a -> List
    // replicate       : Int -> a -> List
    // cycle           : List -> List
    // take            : Int -> List -> List
    // drop            : Int -> List -> List
    // splitAt         : Int -> List -> (List, List)
    // takeWhile       : (a -> Bool) -> List -> List
    // dropWhile       : (a -> Bool) -> List -> List
    // dropWhileEnd    : (a -> Bool) -> List -> List
    // span            : (a -> Bool) -> List -> (List, List)
    // break           : (a -> Bool) -> List -> (List, List)
    // stripPrefix     : Eq a => List -> List -> Maybe List
    // group           : Eq a => List -> [List]
    // inits           : List -> [List]
    // tails           : List -> [List]
    // isPrefixOf      : Eq a => List -> List -> Bool
    // isSuffixOf      : Eq a => List -> List -> Bool
    // isInfixOf       : Eq a => List -> List -> Bool
    // elem            : Eq a => a -> List -> Bool
    // notElem         : Eq a => a -> List -> Bool
    // find            : (a -> Bool) -> List -> Maybe a
    // filter          : (a -> Bool) -> List -> List
    // partition       : (a -> Bool) -> List -> (List, List)
    // (!!)            : List -> Int -> a
    // elemIndex       : Eq a => a -> List -> Maybe Int
    // elemIndices     : Eq a => a -> List -> [Int]
    // findIndex       : (a -> Bool) -> List -> Maybe Int
    // findIndices     : (a -> Bool) -> List -> [Int]
    // lines           : String -> [String]
    // words           : String -> [String]
    // unlines         : [String] -> String
    // unwords         : [String] -> String
    // nub             : Eq a => List -> List
    // delete          : Eq a => a -> List -> List
    // (\\)            : Eq a => List -> List -> List
    // union           : Eq a => List -> List -> List
    // intersect       : Eq a => List -> List -> List
    // sort            : Ord a => List -> List
    // insert          : Ord a => a -> List -> List
    // nubBy           : (a -> a -> Bool) -> List -> List
    // deleteBy        : (a -> a -> Bool) -> a -> List -> List
    // deleteFirstsBy  : (a -> a -> Bool) -> List -> List -> List
    // unionBy         : (a -> a -> Bool) -> List -> List -> List
    // intersectBy     : (a -> a -> Bool) -> List -> List -> List
    // groupBy         : (a -> a -> Bool) -> List -> [List]
    // sortBy          : (a -> a -> Ordering) -> List -> List
    // insertBy        : (a -> a -> Ordering) -> a -> List -> List
    // maximumBy       : (a -> a -> Ordering) -> List -> a
    // minimumBy       : (a -> a -> Ordering) -> List -> a    
}