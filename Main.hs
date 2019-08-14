module Main where

-- ghc lib dependencies
import qualified GHC
import GHC hiding (runGhc, parseModule)
-- import ErrUtils (Messages)
-- import HsSyn (HsModule)
-- import Lexer
-- import Parser
-- import SrcLoc
-- import StringBuffer
-- import FastString
import Outputable hiding ((<>))

-- other dependencies
import GHC.Paths as Paths
-- import System.FilePath.Posix (takeFileName)
import Data.Functor
import Control.Monad.IO.Class
import Options.Applicative
-- import Data.Maybe (fromMaybe)

data Args = Args {
      modPath :: FilePath
    , vbosity :: Int
    } deriving (Show, Eq)

argParser :: Parser Args
argParser = Args
    <$> strArgument (metavar "FILE")
    <*> option auto (
           long "verbosity"
        <> value 1
        <> metavar "[0..5]"
        <> help "verbosity level")

readArgs :: IO Args
readArgs = execParser pInfo
    where
        pInfo = info (argParser <**> helper) fullDesc

setVerbosity :: Int -> Ghc ()
setVerbosity v = do
    dflags <- getSessionDynFlags
    void $ setSessionDynFlags $ dflags {
            verbosity = v
        }

runGhc :: Int -> FilePath -> Ghc a -> IO a
runGhc vb mp doGhc = GHC.runGhc (Just Paths.libdir) $ do
    setVerbosity vb
    target <- guessTarget mp Nothing
    setTargets [target]
    doGhc

put :: Outputable a => a -> Ghc ()
put x = do
    dflags <- getSessionDynFlags
    liftIO $ putStrLn $ showSDoc dflags $ ppr x

main :: IO ()
main = do
    args <- readArgs

    runGhc (vbosity args) (modPath args) $ do
        mg <- depanal [] False
        mapM_ put $ mgModSummaries mg
        pure ()

-- parse :: DynFlags -> (Maybe FilePath) -> StringBuffer -> P a -> ParseResult a
-- parse flags mFn strbuf parser = unP parser parseState
--     where
--         (linenum, colnum) = (1, 1)
--         filename = fromMaybe "<interactive>" mFn
--         location = mkRealSrcLoc (mkFastString filename) linenum colnum
--         parseState = mkPState flags strbuf location
--
-- parseEither :: DynFlags -> (Maybe FilePath) -> StringBuffer -> P a -> Either Messages a
-- parseEither dflags mFn strbuf parser = handleResult $ parse dflags mFn strbuf parser
--     where
--         handleResult result = case result of
--             PFailed errFn _ _ -> Left $ errFn dflags
--             POk _ a -> Right a
--
-- doParse :: DynFlags -> Args -> Ghc (Maybe (Located (HsModule GhcPs)))
-- doParse dflags args = do
--     -- read the module contents and parse it, printing any errors or warnings
--     let modP = modPath args
--         modFn = takeFileName modP
--     inBuffer <- liftIO $ hGetStringBuffer modP
--
--     case parseEither dflags (pure modFn) inBuffer parseModule of
--         Left (warns, errs) -> do
--             liftIO $ mapM_ print warns
--             liftIO $ mapM_ print errs
--             pure Nothing
--         Right parsedMod -> do
--             liftIO $ putStrLn "parsing succeeded"
--             pure (Just parsedMod)
--
