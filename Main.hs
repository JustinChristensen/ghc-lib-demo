module Main where

-- ghc lib dependencies
import qualified GHC
import GHC (Ghc, GhcPs, DynFlags, getSessionDynFlags,
            guessTarget, setTargets)
import ErrUtils (Messages)
import HsSyn (HsModule)
import StringBuffer
import Lexer
import Parser
import SrcLoc
import FastString

-- other dependencies
import GHC.Paths as Paths
import System.FilePath.Posix (takeFileName)
import Control.Monad.IO.Class
import Options.Applicative
import Data.Maybe (fromMaybe)

data Args = Args {
        modPath :: FilePath
    } deriving (Show, Eq)

argParser :: Parser Args
argParser = Args <$> strArgument mempty

readArgs :: IO Args
readArgs = execParser pInfo
    where
        pInfo = info (argParser <**> helper) fullDesc

runGhc :: Ghc a -> IO a
runGhc doGhc = GHC.runGhc (Just Paths.libdir) $ do
    args <- liftIO readArgs
    target <- guessTarget (modPath args) Nothing
    setTargets [target]
    doGhc

parse :: DynFlags -> (Maybe FilePath) -> StringBuffer -> P a -> ParseResult a
parse flags mFn strbuf parser = unP parser parseState
    where
        (linenum, colnum) = (1, 1)
        filename = fromMaybe "<interactive>" mFn
        location = mkRealSrcLoc (mkFastString filename) linenum colnum
        parseState = mkPState flags strbuf location

parseEither :: DynFlags -> (Maybe FilePath) -> StringBuffer -> P a -> Either Messages a
parseEither dflags mFn strbuf parser = handleResult $ parse dflags mFn strbuf parser
    where
        handleResult result = case result of
            PFailed errFn _ _ -> Left $ errFn dflags
            POk _ a -> Right a

doParse :: DynFlags -> Args -> Ghc (Maybe (Located (HsModule GhcPs)))
doParse dflags args = do
    -- read the module contents and parse it, printing any errors or warnings
    let modP = modPath args
        modFn = takeFileName modP
    inBuffer <- liftIO $ hGetStringBuffer modP

    case parseEither dflags (pure modFn) inBuffer parseModule of
        Left (warns, errs) -> do
            liftIO $ mapM_ print warns
            liftIO $ mapM_ print errs
            pure Nothing
        Right parsedMod -> do
            liftIO $ putStrLn "parsing succeeded"
            pure (Just parsedMod)

main :: IO ()
main = do
    args <- readArgs

    runGhc $ do
        -- get the dynamic flags for this session
        dflags <- getSessionDynFlags

        -- parse the module
        -- _ <- doParse dflags args

        pure ()


