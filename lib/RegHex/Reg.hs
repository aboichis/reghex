module RegHex.Reg where

import RegHex.Semirings
-- import Data.Word(Word8(..))
import Prelude hiding (seq)
import RegHex.Misc
-- import qualified Data.Vector as V
import qualified Data.Primitive.SmallArray as A

-- data Reg s c = MkAnn 
--     { final :: !s
--     , reg :: !(Reg' s c)
--     } deriving Show

type Reg s c = Ann s (Reg' s c)

data Reg' s c r
    = Eps
    | Lit !(A.SmallArray (Int -> c -> s)) !(Int -> c -> s) !(A.SmallArray s)
    | Alt !r !r !s -- s = is-empty
    | Seq !r !r !s -- s = is-empty
    | Rep !r

instance (Show s, Show c, Show r) => Show (Reg' s c r) where
    show Eps = "Eps"
    show (Lit _ _ _) = "Lit <..>"
    show (Alt l r e) = "Alt (" ++ show l ++ ") (" ++ show r ++ ") (" ++ show e ++ ")"
    show (Seq l r e) = "Seq (" ++ show l ++ ") (" ++ show r ++ ") (" ++ show e ++ ")"
    show (Rep r) = "Rep (" ++ show r ++ ")"

empty :: Semiring s => Reg' s c r -> s
empty = \case 
    Eps -> one
    Lit _ _ _ -> zero
    Alt _ _ s -> s
    Seq _ _ s -> s
    Rep _ -> one

eps :: Semiring s => Reg s c
eps = MkAnn zero Eps
-- lit w f i c = MkAnn (w .*  f i c) (Lit V.empty f V.empty)
-- lit0 f = MkAnn zero (Lit V.empty f (V.empty))
-- litEq0 :: (Eq c, SemiringIdx s) => c -> Reg s c
-- litEq0 c = MkAnn zero (Lit V.empty  (\i x -> if c == x then index i else zero) V.empty)
alt p q = MkAnn zero (Alt p q (empty (recur p) .+ empty (recur q)))
seq p q = MkAnn zero (Seq p q (empty (recur p) .* empty (recur q)))
rep r = MkAnn (ann r) (Rep r)