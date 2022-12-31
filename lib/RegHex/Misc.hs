{-# LANGUAGE UndecidableInstances #-}
module RegHex.Misc where

data Ann a f = MkAnn
    { ann :: !a
    , recur :: !(f (Ann a f))
    }
deriving instance (Show (f (Ann a f)), Show a) => Show (Ann a f)