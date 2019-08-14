# GHC Compiler Pipeline Demonstration

-- The pipeline, according to the wiki
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

In reality, #2 and #3 are tightly coupled, see:
https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/typecheck/TcRnTypes.hs#L254

## Notes and References

https://gitlab.haskell.org/ghc/ghc/tree/ghc-8.6.5-release

### Compiler Pipeline Types

* HsSyn 
  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/hsSyn/HsExpr.hs#L274
  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/hsSyn/HsSyn.hs#L66

* CoreExpr 
  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/coreSyn/CoreSyn.hs#L270 

* STG
  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/stgSyn/StgSyn.hs#L197

### References

1. https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/hsc-main  
2. http://hackage.haskell.org/package/ghc-8.6.5  
3. https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#equality-constraints-coercible-and-the-kind-constraint
