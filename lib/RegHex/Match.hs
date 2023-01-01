{-# LANGUAGE UnboxedTuples #-}
module RegHex.Match(match, matchBytes) where

import RegHex.Semirings
import RegHex.Reg
import RegHex.Misc
-- import qualified Data.Vector as V
import qualified Data.Primitive.SmallArray as A
import qualified Data.ByteString as B
import Data.Word
import Data.Foldable

{-# INLINEABLE shift #-}
shift :: forall s c. Semiring s => Int -> c -> s -> Reg' s c (Ann s (Reg' s c)) -> Reg s c
shift i c m = \case
    Eps -> MkAnn m Eps
    Lit fs f ws ->
        let !n = A.sizeofSmallArray fs
            !shiftWs = if n == 0 then ws else A.createSmallArray n (error "impossible") $ \arr -> do --(\j -> let !r = getW m ws (j-1) .* V.unsafeIndex fs j i c in r)
                for_ [0..n - 1] $ \j ->
                    A.writeSmallArray arr j $! (getArrDefault m ws (j-1) .* A.indexSmallArray fs j i c)
        in MkAnn (getArrDefault m ws (n-1) .* f i c) (Lit fs f shiftWs)
    Alt (MkAnn _ p) (MkAnn _ q) e -> 
        let !p' = shift i c m p
            !q' = shift i c m q
        in MkAnn (ann p' .+ ann q') (Alt p' q' e)
    Seq (MkAnn wp p) (MkAnn _ q) e ->
        let !p' = shift i c m p
            !q' = shift i c ((m .* empty p) .+ wp) q
        in MkAnn ((ann p' .* empty q) .+ ann q') (Seq p' q' e)
    Rep (MkAnn w r) ->
        let !r' = shift i c (m .+ w) r
        in MkAnn (ann r') (Rep r')

getArrDefault m arr i = case i of
    -1 -> m
    _ -> A.indexSmallArray arr i

{-# INLINEABLE match #-}
match :: Semiring s => Reg s c -> [c] -> s
match (MkAnn _ p) xs = fst $ ifoldl' step (empty p, p) xs
    where
    step (!m, !r) i c = 
        let !(MkAnn w r') = shift i c one r
        in (m .+ w .+ empty r, r')

{-# INLINEABLE matchBytes #-}
matchBytes :: Semiring s => Reg s Word8 -> IO B.ByteString -> IO s
matchBytes (MkAnn _ p) inp = go p (empty p) 0
    where
        go !q !s !i = do
            !cs <- inp
            if B.null cs
            then pure s
            else 
                let 
                !(# !s', !p', !i' #) = B.foldr (\ x k !j !y !z -> 
                    let 
                    !(# !y', !z' #) = step y z j x
                    in k (j + 1) y' z') (\ !j !y !z -> (# y, z, j #)) cs i s q
                in go p' s' i'
        step !s !r !i c = 
            case shift i c one r of
                MkAnn w r' -> (# s .+ w .+ empty r, r' #)

{-# INLINEABLE ifoldl' #-}
ifoldl' :: Foldable f => (b -> Int -> a -> b) -> b -> f a -> b
ifoldl' f z0 xs = foldr (\ !x k !z !i -> k (f z i x) (i+1)) (\z _ -> z) xs z0 0