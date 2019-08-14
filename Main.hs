module Main where

import GHC
import GHC.Paths as Paths
import Lexer
import SrcLoc
import FastString
import Parser
import HsDumpAst
import StringBuffer
import Outputable
import ErrUtils (Messages)
import Control.Monad.IO.Class

runParser :: DynFlags -> String -> P a -> ParseResult a
runParser flags str parser = unP parser parseState
    where
      filename = "<interactive>"
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
      parseState = mkPState flags buffer location

testExpression :: DynFlags -> String -> Either Messages (Located (HsExpr GhcPs))
testExpression dflags str =
    let presult = runParser dflags str parseExpression
    in case presult of
        POk _ a -> Right a
        PFailed errFn _ _ -> Left $ errFn dflags

main :: IO ()
main =
    GHC.runGhc (Just Paths.libdir) $ do
        dflags <- GHC.getSessionDynFlags
        pure ()






