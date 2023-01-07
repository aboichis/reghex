module Convert where

import Prelude hiding (seq)
import qualified Parse
import RegHex.Reg
import RegHex.Semirings
import RegHex.Misc
import qualified Data.Sequence as Seq
-- import qualified Data.Vector as V
import qualified Data.Primitive.SmallArray as A
import Data.Foldable
import Data.Word
import qualified Data.Primitive.PrimArray as PA
import Control.Monad.ST (runST)
import Data.Foldable.WithIndex


convert :: SemiringIdx s => Parse.Re -> Reg s Word8
convert = \case
    Parse.Eps -> eps
    Parse.Basic c -> convert $ Parse.BasicSeq (Seq.singleton c)
    Parse.BasicSeq (cs Seq.:|> c) -> MkAnn zero (Lit fs (toF c) (A.createSmallArray (Seq.length cs) (zero) (\_ -> pure ())))
        where
        toF (Parse.Lit x) = \i d -> if d == x then index i else zero
        toF Parse.Dot = \i _ -> index i
        
        fs = A.smallArrayFromListN (Seq.length cs) . fmap toF . toList $ cs
    Parse.BasicSeq Seq.Empty -> error "BasicSeq shouldn't have empty sequence!"
    Parse.Seq p q qs -> foldl' (\r -> seq r . convert) (seq (convert p) (convert q)) qs
    Parse.Alt p q qs -> foldl' (\r -> alt r . convert) (alt (convert p) (convert q)) qs
    Parse.Rep p -> rep (convert p)


allLit :: Parse.Re -> Maybe (PA.PrimArray Word8)
allLit = \case
    Parse.BasicSeq cs -> runST $ do
        arr <- PA.newPrimArray (Seq.length cs)
        ifoldr 
            (\i c k -> case c of
                Parse.Lit x -> PA.writePrimArray arr i x >> k
                _ -> pure Nothing) 
            (Just <$> PA.unsafeFreezePrimArray arr) 
            cs
    _ -> Nothing
