module KMP(matchKMP) where
import qualified Data.Primitive.PrimArray as A
import Data.Word
import qualified Pipes as P
import qualified Data.ByteString as B

getJumpTable :: A.PrimArray Word8 -> A.PrimArray Int
getJumpTable pat = A.runPrimArray $ do
    arr <- A.newPrimArray m
    A.writePrimArray arr 0 0
    go arr 0 1
    where
    m = A.sizeofPrimArray pat
    go arr _ !q | q == m = pure arr
    go arr !k !q = do
        !k' <- getNewK pat k arr q
        A.writePrimArray arr q k'
        go arr k' (q+1)
    
getNewK pat !k arr !q
    | A.indexPrimArray pat k == A.indexPrimArray pat q = pure $ k + 1
    | k /= 0 = do 
        !k' <- A.readPrimArray arr (k-1)
        getNewK pat k' arr q
    | otherwise = pure k

getNewQ pat !q arr x
    | A.indexPrimArray pat q == x = q + 1
    | q /= 0 = getNewQ pat (A.indexPrimArray arr (q-1)) arr x
    | otherwise = q

matchKMPChunk :: Functor m => Int -> Int -> A.PrimArray Word8 -> A.PrimArray Int -> B.ByteString -> P.Producer Int m (Int, Int)
matchKMPChunk i q0 pat jmps bs = B.foldr step (\ !j !q -> pure (j,q)) bs i q0
    where
    m = A.sizeofPrimArray pat
    step x k !j !q =
        let !q' = getNewQ pat q jmps x
        in if q' == m
        then P.yield (j-m+1) >> k (j+1) (A.indexPrimArray jmps (q' - 1))
        else k (j+1) q'

{-# INLINEABLE matchKMP #-}
matchKMP :: A.PrimArray Word8 -> IO B.ByteString -> P.Producer Int IO ()
matchKMP pat getChunk = go 0 0
    where 
    go i q = do
        bs <- P.liftIO getChunk
        if B.null bs
            then pure ()
            else do
            (!i', !q') <- matchKMPChunk i q pat jmps bs
            go i' q'
    jmps = getJumpTable pat
