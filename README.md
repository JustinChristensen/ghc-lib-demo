# GHC Compiler Pipeline Demonstration

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
