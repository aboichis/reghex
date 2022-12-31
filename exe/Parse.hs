{-# LANGUAGE UnboxedTuples #-}
module Parse(Re(..), Single(..), regex', compacted) where

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Bits
import Data.Char
import Data.Text(Text)
import Data.Word(Word8)
import Data.Void
import qualified Data.Sequence as Seq
import Data.Foldable

data Re
    = Eps
    | Basic Single
    | BasicSeq (Seq.Seq Single)
    | Seq Re Re [Re]
    | Alt Re Re [Re]
    | Rep Re
    deriving Show

data Single 
    = Dot
    | Lit !Word8
    deriving Show

data Modifer = Star | Plus | Question | Repl Int

parseSpace :: (MonadParsec e s m, Token s ~ Char) => m ()
parseSpace = L.space C.space1 empty empty

hexByte :: Parsec Void Text Word8
hexByte = lexeme $ (\x y -> fromIntegral $ shiftL (digitToInt x) 4 .|. digitToInt y) <$> C.hexDigitChar <*> C.hexDigitChar

lexeme = L.lexeme parseSpace
symbol = L.symbol parseSpace

parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

decimal = lexeme L.decimal

plus,star,question,dot,bar :: Parsec Void Text Text
plus = symbol "+"
star = symbol "*"
question = symbol "?"
dot = symbol "."
bar = symbol "|"

replicateRe n r 
    | n <= 0 = Eps
    | n == 1 = r
    | otherwise = Seq r r (replicate (n-2) r)

regex = listToTree Alt <$> sepBy1 alt bar
alt = listToTree Seq <$> many sequent

sequent = do
    s <- parens regex <|> basic
    m <- optional modifier
    pure $ case m of
        Nothing -> s
        Just t -> case t of 
            Star -> Rep s
            Plus -> Seq s (Rep s) []
            Question -> Alt s Eps []
            Repl n -> replicateRe n s

basic = Basic <$> ((Lit <$> hexByte) <|> (Dot <$ dot))
modifier = 
    (Plus <$ plus) 
    <|> (Star <$ star) 
    <|> (Question <$ question) 
    <|> (do n <- braces decimal
            if toInteger (maxBound @Int) < n
                then fail $ "Repetition overflow (must fit in machine sized signed integer)"
                else pure (Repl (fromInteger n))
        )

regex' = regex <* eof

compacted = compact <$> regex

listToTree con xs = case xs of
        [] -> Eps
        [x] -> x
        (x:y:r) -> con x y r

seqToTree con xs = case xs of
    Seq.Empty -> Eps
    (x Seq.:<| Seq.Empty) -> x
    (x Seq.:<| y Seq.:<| r) -> con x y (toList r)

compact :: Re -> Re
compact = \case 
    s@(Seq _ _ _) -> seqToTree Seq (compactSeq s)
    Alt p q rs -> Alt (compact p) (compact q) (compact <$> rs)
    Rep s -> Rep (compact s)
    b@(Basic _) -> b
    b@(BasicSeq _) -> b
    Eps -> Eps

compactSeq r = case go ( Seq.empty, Seq.empty ) r of
    ( basics, all ) -> appendNull basics all
    where
    appendNull basics all = if Seq.null basics then all else all Seq.|> BasicSeq basics
    go acc@( !basics, !all ) = \case
        Eps -> acc
        Basic x -> ( basics Seq.|> x, all )
        r@(Alt _ _ _) -> ( Seq.empty, appendNull basics all Seq.|> compact r )
        r@(Rep _) -> ( Seq.empty, appendNull basics all Seq.|> compact r )
        Seq p q rs -> foldl' go (go (go acc p) q) rs
        BasicSeq s ->  ( basics Seq.>< s, all ) -- error "compactSeq argument shouldn't have BasicSeq"