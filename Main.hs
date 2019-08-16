module Main where

-- ghc lib dependencies
import qualified GHC
import GHC hiding (runGhc, parseModule)
import ErrUtils (Messages)
import HscMain
import HscTypes
import TcRnDriver
import Outputable hiding ((<>))
-- import HsSyn (HsModule)
-- import Lexer
-- import Parser
-- import StringBuffer
-- import FastString

-- other dependencies
import GHC.Paths as Paths
import Data.Functor
import Control.Monad.IO.Class
import System.Environment (getArgs)

readArgs :: Ghc [String]
readArgs = do
    args <- liftIO getArgs
    let locArgs = mkGeneralLocated "cmdline" <$> args
    dflags' <- getSessionDynFlags
    (dflags, files, warns) <- parseDynamicFlags dflags' locArgs
    liftIO $ handleFlagWarnings dflags warns
    void $ setSessionDynFlags dflags
    pure $ unLoc <$> files

runGhc :: Ghc a -> IO a
runGhc doGhc = GHC.runGhc (Just Paths.libdir) $ do
    files <- readArgs
    targets <- mapM (flip guessTarget Nothing) files
    setTargets targets
    doGhc

put :: Outputable a => a -> Ghc ()
put x = do
    dflags <- getSessionDynFlags
    liftIO $ putStrLn $ showSDoc dflags $ ppr x

logErrsAndWarns :: IO (Messages, a) -> IO a
logErrsAndWarns act = do
    ((warns, errs), x) <- act
    liftIO $ mapM_ print warns
    liftIO $ mapM_ print errs
    pure x

main :: IO ()
main = do
    runGhc $ do
        -- analyze our module dependencies, storing the module graph in the session
        mg <- depanal [] False

        hscEnv <- getSession

        -- print GHC's idea of what the module graph looks like
        let mss = mgModSummaries mg
        mapM_ put mss

        -- pluck the first module summary and parse it
        ms <- firstMS
        parsedMod <- liftIO $ hscParse hscEnv ms

        -- rename and typecheck
        mTcGblEnv <- liftIO $ logErrsAndWarns $ tcRnModule hscEnv ms False parsedMod

        case mTcGblEnv of
            Just _ -> pure ()
            _ -> liftIO $ putStrLn "No typechecker env returned"

        pure ()
    where
        firstMS = do
            mg <- getModuleGraph
            pure $ head $ mgModSummaries mg

