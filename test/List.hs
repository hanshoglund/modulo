module List where
import Foreign
import Foreign.C
import Std
 
data Elem_
 
type Elem = Ptr Elem_
 
data Filter_
 
type Filter = Ptr Filter_
 
data Equal_
 
type Equal = Ptr Equal_
 
data Order_
 
type Order = Ptr Order_
 
data Number_
 
type Number = Ptr Number_
 
data Builder_
 
type Builder = Ptr Builder_
 
data List_
 
type List = Ptr List_
 
foreign import ccall unsafe "list_nil" nil :: IO List
 
foreign import ccall unsafe "list_cons" cons ::
        Elem -> List -> IO List
 
foreign import ccall unsafe "list_snoc" snoc ::
        Elem -> List -> IO List
 
foreign import ccall unsafe "list_head" head :: List -> IO Elem
 
foreign import ccall unsafe "list_tail" tail :: List -> IO List
 
foreign import ccall unsafe "list_init" init :: List -> IO List
 
foreign import ccall unsafe "list_last" last :: List -> IO Elem
 
foreign import ccall unsafe "list_empty" empty :: List -> IO CInt
 
foreign import ccall unsafe "list_lenght" lenght :: List -> IO CInt
 
foreign import ccall unsafe "list_sort" sort ::
        Order -> List -> IO List
 
foreign import ccall unsafe "list_reverse" reverse ::
        List -> IO List
 
foreign import ccall unsafe "list_intersperse" intersperse ::
        Elem -> List -> IO List
 
foreign import ccall unsafe "list_build" build :: IO Builder
 
foreign import ccall unsafe "list_builder_add" builderAdd ::
        Elem -> Builder -> IO ()
 
foreign import ccall unsafe "list_builder_finish" builderFinish ::
        Builder -> IO List
 
foreign import ccall unsafe "list_transpose" transpose ::
        List -> IO List
 
foreign import ccall unsafe "list_subsequences" subsequences ::
        List -> IO List
 
foreign import ccall unsafe "list_permutations" permutations ::
        List -> IO List
 
foreign import ccall unsafe "list_concat" concat :: List -> IO List
 
foreign import ccall unsafe "list_and" and :: List -> IO CInt
 
foreign import ccall unsafe "list_or" or :: List -> IO CInt
 
foreign import ccall unsafe "list_any" any ::
        Filter -> List -> IO CInt
 
foreign import ccall unsafe "list_all" all ::
        Filter -> List -> IO CInt
 
foreign import ccall unsafe "list_sum" sum ::
        Number -> List -> IO Elem
 
foreign import ccall unsafe "list_product" product ::
        Number -> List -> IO Elem
 
foreign import ccall unsafe "list_maximum" maximum ::
        Order -> List -> IO Elem
 
foreign import ccall unsafe "list_minimum" minimum ::
        Order -> List -> IO Elem
 
foreign import ccall unsafe "list_repeat" repeat :: Elem -> IO List
 
foreign import ccall unsafe "list_replicate" replicate ::
        CInt -> Elem -> IO List
 
foreign import ccall unsafe "list_cycle" cycle :: List -> IO List
 
foreign import ccall unsafe "list_take" take ::
        CInt -> List -> IO List
 
foreign import ccall unsafe "list_drop" drop ::
        CInt -> List -> IO List
 
foreign import ccall unsafe "list_is_prefix_of" isPrefixOf ::
        Equal -> List -> List -> IO CInt
 
foreign import ccall unsafe "list_is_suffix_of" isSuffixOf ::
        Equal -> List -> List -> IO CInt
 
foreign import ccall unsafe "list_is_infix_of" isInfixOf ::
        Equal -> List -> List -> IO CInt
 
foreign import ccall unsafe "list_elem" elem ::
        Equal -> Elem -> List -> IO CInt
 
foreign import ccall unsafe "list_not_elem" notElem ::
        Equal -> Elem -> List -> IO CInt

