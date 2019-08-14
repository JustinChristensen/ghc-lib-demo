module Main where

-- ghc lib dependencies
import qualified GHC
import GHC (Ghc, DynFlags, getSessionDynFlags)
import ErrUtils (Messages)
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

-- 1. M.hs          -> Parse -> HsSyn RdrName
-- 2. HsSyn RdrName -> Rename -> HsSyn Name
-- 3. HsSyn Name    -> Typecheck -> HsSyn Id
-- 4. HsSyn Id      -> Desugar -> CoreExpr
-- 5. CoreExpr      -> Simplify[N] -> CoreExpr
-- 6. CoreExpr      -> CoreTidy -> CoreExpr
-- 7. CoreExpr      -> CorePrep -> CoreExpr
-- 8. CoreExpr      -> To STG -> STG
-- 9. STG           -> Codegen -> Cmm
-- 10. Cmm          -> Machine Code -> M.s

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
runGhc = GHC.runGhc (Just Paths.libdir)

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

main :: IO ()
main = do
    args <- readArgs

    runGhc $ do
        -- get the dynamic flags for this session
        dflags <- getSessionDynFlags

        -- read the module contents and parse it, printing any errors or warnings
        let modP = modPath args
            modFn = takeFileName modP
        inBuffer <- liftIO $ hGetStringBuffer modP
        case parseEither dflags (pure modFn) inBuffer parseModule of
            Left (warns, errs) -> do
                liftIO $ mapM_ print warns
                liftIO $ mapM_ print errs
            Right parsedMod -> pure ()

{-
ghc --show-options

HscMain

After parse
-ddump-parsed
-ddump-parsed-ast
-dsource-stats


-}



