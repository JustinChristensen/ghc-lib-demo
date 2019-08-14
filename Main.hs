{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text.IO as I (putStrLn)

-- import GHC
-- import GHC.Paths as Paths
-- import Lexer
-- import SrcLoc
-- import FastString
-- import Parser
-- import HsDumpAst
-- import StringBuffer
-- import Outputable
-- import ErrUtils (Messages)
-- import Control.Monad.IO.Class
--
-- runParser :: DynFlags -> String -> P a -> ParseResult a
-- runParser flags str parser = unP parser parseState
--     where
--       filename = "<interactive>"
--       location = mkRealSrcLoc (mkFastString filename) 1 1
--       buffer = stringToStringBuffer str
--       parseState = mkPState flags buffer location

-- type LHsExpr p = Located (HsExpr p) 	-- Defined in ‘HsExpr’
-- type GhcPs = GhcPass 'Parsed
-- data Pass = Parsed | Renamed | Typechecked
-- Located (HsExpr (GhcPass 'Parsed))
-- testExpression :: DynFlags -> String -> Either Messages (Located (HsExpr GhcPs))
-- testExpression dflags str =
--     let presult = runParser dflags str parseExpression
--     in case presult of
--         POk _ a -> Right a
--         PFailed errFn _ _ -> Left $ errFn dflags

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

-- main :: IO ()
-- main =
--     GHC.runGhc (Just Paths.libdir) $ do
--         dflags <- GHC.getSessionDynFlags
--         pure ()

{-
ghc --show-options

HscMain

After parse
-ddump-parsed
-ddump-parsed-ast
-dsource-stats


-}

main :: IO ()
main = I.putStrLn ("Hello, World!" :: Text)


