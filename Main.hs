module Main where

-- ghc lib dependencies
import qualified GHC
import GHC hiding (runGhc, parseModule)
import ErrUtils (Messages)
import HscMain
import HscTypes
import TcRnDriver
import TcRnTypes
import Outputable hiding ((<>))
-- import HsSyn (HsModule)
-- import Lexer
-- import Parser
-- import StringBuffer
-- import FastString

-- other dependencies
import GHC.Paths as Paths
import Data.Functor
import Data.Graph
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class
import System.Environment (getArgs)

putStrErr :: String -> IO ()
putStrErr = hPutStrLn stderr

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
        liftIO $ mapM_ putB warns
        liftIO $ mapM_ putB errs
        pure x
    where
        b s = "\27[1m" ++ s ++ "\27[0m"
        putB = putStrLn . b . show

runPL :: SCC ModSummary -> Ghc (Maybe TcGblEnv)
runPL (CyclicSCC _) = undefined
runPL (AcyclicSCC ms) = do
    hscEnv <- getSession
    parsedMod <- liftIO $ hscParse hscEnv ms
    liftIO $ logErrsAndWarns $ tcRnModule hscEnv ms True parsedMod

main :: IO ()
main = main' $> ()

main' :: IO [TcGblEnv]
main' = do
    runGhc $ do
        -- analyze our module dependencies, storing the module graph in the session
        mg <- depanal [] False

        -- print GHC's idea of what the initial module graph looks like
        liftIO $ putStrLn "Before sort"
        printMss (mgModSummaries mg)

        -- topologically sort the module graph
        let sortedMg = topSortModuleGraph False mg (Just $ mkModuleName "Main")

        liftIO $ putStrLn "After sort"
        printMss $ [ms | AcyclicSCC ms <- sortedMg]

        -- loop through the module summaries and parse/rename/typecheck
        fmap catMaybes $ forM sortedMg runPL

    where
        printMss = mapM_ put

