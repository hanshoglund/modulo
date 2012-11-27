
/**
    @defgroup SclThread Threads
    @brief    
        Provides cross-platform thread support.
 */
module Scl.Thread
{
    import Std;
    
    type Thread = struct { x : Int32 };
    type Mutex = struct { x : Int32 };
    type Lock = struct { x : Int32 };
    type Id = Int64;

    type Runnable = (Id) -> Void;

    create : (Runnable) -> Thread;
}