module Main(main) where

import Parse
import Convert
import qualified Options.Applicative as O
import qualified Data.Text as T
import System.FilePath
import Control.Applicative
import Data.Bifunctor
import qualified Text.Megaparsec as P
import System.Directory
import Data.Maybe
import Data.Foldable
import qualified Data.ByteString as B
import System.IO
import qualified Data.IntSet as S
import Data.Streaming.Filesystem
import Control.Exception (bracket, IOException, catch, try)
import Data.Functor
import qualified Data.Sequence as Seq
import Control.Monad
import RegHex.Semirings
import RegHex.Match
import KMP
import qualified Pipes.Prelude as P
import qualified Pipes as P
import Control.Monad.Trans.Class(lift)


data MatchType = MatchBool | MatchOffsets deriving Show
data PathType = Dir | File deriving Show
data Args = MkArgs !(Maybe MatchType) !Re !(Maybe FilePath)

optsParser = MkArgs <$> matchTypeParser <*> hexParser <*> pathParser
  where
  matchTypeParser = optional $
    O.flag' MatchBool (O.short 'b' <> O.help "bool match") 
    <|> O.flag' MatchOffsets (O.short 'o' <> O.help "offset match")
  
  hexParser = 
    O.argument
    (O.eitherReader $ first (("Error! Invalid regex:\n" ++) . P.errorBundlePretty) . P.runParser compacted "" . T.pack)
    (O.metavar "regex" <> O.help "regex string")

  pathParser = optional $ 
    O.argument 
      (O.eitherReader $ \s ->
        if isValid s 
          then Right s 
          else Left ("Error! Invalid path: " ++ s))
      (O.metavar "path" <> O.help "path to search (file or dir)")

runOptParser = 
  O.customExecParser 
    (O.prefs O.showHelpOnError)
    (O.info (optsParser <**> O.helper) mempty)

resolveMatchPath :: Maybe MatchType -> Maybe FilePath -> IO (Either String (MatchType, PathType, FilePath))
resolveMatchPath m Nothing =  Right . (fromMaybe MatchBool m, Dir, ) <$> getCurrentDirectory 
resolveMatchPath m (Just p) =
  doesFileExist p >>= \case
    True -> pure $ Right $ (resolveMatch m File, File, p)
    False -> doesDirectoryExist p <&> \case 
      True -> Right $ (resolveMatch m Dir, Dir, p)
      False -> Left $ "Error! Path does not exist: " ++ show p
  where
  resolveMatch Nothing File = MatchOffsets
  resolveMatch Nothing Dir = MatchBool
  resolveMatch (Just m) _ = m

main :: IO ()
main = do
  MkArgs matchMay regexSyn pathMay <- runOptParser
  resolveMatchPath matchMay pathMay >>= \case
    Left e -> putStrLn e
    Right (matchType, pathType, path) -> case allLit regexSyn of
      Just kmpPat -> runKMP kmpPat matchType pathType path
      Nothing -> case matchType of
        MatchBool -> do
          let !regexSem = convert @Bool regexSyn
          case pathType of
            Dir -> runDir path $ \f -> do
              r <- runRegex regexSem f
              when r (putStrLn f)
            File -> do 
              r <- runRegex regexSem path
              print r
        MatchOffsets -> do
          let !regexSem = convert @AllOffset regexSyn
          case pathType of
            Dir -> runDir path $ \f -> do
              MkAllOffset set <- runRegex regexSem f
              for_ (S.toList set) (\r -> putStrLn $ f ++ ": " ++ show r)
            File -> do
              MkAllOffset set <- runRegex regexSem path
              for_ (S.toList set) print

runKMP pat matchType pathType path =
  case pathType of 
    File -> runKMPFile pat path actFile
    Dir -> runDir path (\file -> runKMPFile pat file (actDir file))
  where
  actFile = case matchType of 
    MatchBool -> P.null >=> \b -> print (not b)
    MatchOffsets -> \p -> P.runEffect $ P.for p (lift . print)
      
  actDir file = case matchType of
    MatchBool -> P.null >=> \b -> unless b (putStrLn file)
    MatchOffsets -> \p -> P.runEffect $ P.for p (\x -> lift $ putStrLn $ file ++ ": " ++ show x)

runKMPFile pat path act = withBinaryFile path ReadMode (\hndl -> act $ matchKMP pat (B.hGetSome hndl 65536))

-- runRegex :: Semiring s => Reg s Word8 -> FilePath -> IO s
runRegex regex path = withBinaryFile path ReadMode (\hndl -> matchBytes regex (B.hGetSome hndl 65536))

runDir :: FilePath -> (FilePath -> IO ()) -> IO ()
runDir f act = loop (Seq.singleton f)
  where
  loop :: Seq.Seq FilePath -> IO ()
  loop Seq.Empty = pure ()
  loop (d Seq.:<| dirs) = do
    leftoverDirs <- bracket (try @IOException $ openDirStream d) (either (const $ pure ()) closeDirStream) $ \me -> do
      case me of 
        Left e -> do
          hPutStrLn stderr $ "ERROR!: Couldn't enumerate directory \"" ++ d ++ "\": " ++ show e
          pure dirs
        Right stream -> dirLoop d dirs stream
    loop leftoverDirs

  dirLoop :: FilePath -> Seq.Seq FilePath -> DirStream -> IO (Seq.Seq FilePath)
  dirLoop path initialDirs stream = go initialDirs
    where 
    go :: Seq.Seq FilePath -> IO (Seq.Seq FilePath)
    go !dirs = catch @IOException (readDirStream stream) (\e -> print e $> Nothing) >>= \case
      Nothing -> pure dirs
      Just entry -> 
        let fullEntry = path </> entry
        in try @IOException (getFileType fullEntry) >>= \case
            Left e -> do
              hPutStrLn stderr $ "ERROR!: While searching file " ++ fullEntry ++ "! " ++ show e
              go dirs
            Right ft -> case ft of 
              FTFile -> do
                catch @IOException (act fullEntry) 
                  (\e -> hPutStrLn stderr $ "ERROR!: While searching file " ++ fullEntry ++ "! " ++ show e)
                go dirs
              FTDirectory -> go (dirs Seq.:|> fullEntry)
              _ -> go dirs