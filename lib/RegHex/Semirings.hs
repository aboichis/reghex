module RegHex.Semirings where

import Numeric.Natural
import qualified Data.IntSet as SI

class Semiring s where
    one :: s
    zero :: s
    (.+) :: s -> s -> s
    (.*) :: s -> s -> s

class Semiring s => SemiringIdx s where
    index :: Int -> s

instance Semiring Bool where
    {-# INLINEABLE one #-}
    one = True
    {-# INLINEABLE  zero #-}
    zero = False
    {-# INLINEABLE (.+) #-}
    (.+) = (||)
    {-# INLINEABLE (.*) #-}
    (.*) = (&&)

instance SemiringIdx Bool where
    {-# INLINEABLE index #-}
    index _ = one

newtype Count = MkCount Natural

instance Semiring Count where
    one = MkCount 1
    zero = MkCount 0
    (MkCount n) .+ (MkCount m) = MkCount (n + m)
    (MkCount n) .* (MkCount m) = MkCount (n * m)

instance SemiringIdx Count where
    index _ = one

newtype AllOffset = MkAllOffset SI.IntSet deriving Show

instance Semiring AllOffset where
    {-# INLINEABLE zero #-}
    zero = MkAllOffset $ SI.empty
    {-# INLINEABLE one #-}
    one = MkAllOffset $ SI.singleton (-1)
    {-# INLINEABLE (.+) #-}
    (MkAllOffset s1) .+ (MkAllOffset s2) = MkAllOffset (SI.union s1 s2)
    {-# INLINEABLE (.*) #-}
    (MkAllOffset s1) .* (MkAllOffset s2)
        | SI.null s1 || SI.null s2 = MkAllOffset SI.empty
        | otherwise =  MkAllOffset $ if SI.member (-1) s1 then SI.union (SI.delete (-1) s1) s2 else s1
        -- let foldl' xs z f = SI.foldl' f z xs
        -- in MkAllOffset $ 
        --     foldl' s1 SI.empty $ \s i -> 
        --         foldl' s2 s $ \s j ->
        --             if i == -1                     
        --             then SI.insert j s 
        --             else SI.insert i s

instance SemiringIdx AllOffset where
    {-# INLINEABLE index #-}
    index i = MkAllOffset $ SI.singleton i