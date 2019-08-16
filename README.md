# GHC Compiler Pipeline Demonstration

```
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
```

In reality, #2 and #3 are tightly coupled, see:
https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/typecheck/TcRnTypes.hs#L254

## Notes and References

* https://gitlab.haskell.org/ghc/ghc/tree/ghc-8.6.5-release
* Typechecker and Renamer Entrypoint:
  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/typecheck/TcRnDriver.hs#L152

### Compiler Pipeline Types

* HsSyn 
  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/hsSyn/HsExpr.hs#L274
  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/hsSyn/HsSyn.hs#L66

* CoreExpr 
  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/coreSyn/CoreSyn.hs#L270 

* STG
  https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/stgSyn/StgSyn.hs#L197

### Terminology

* Ghc Monad
  Main monad for compiling source files. Handles dynamic flags, targets, and a session that 
  contains a graph of of the modules and their dependencies. See [GhcMonad.hs].
  
* Module Graph
  Created by depanal in [GhcMake.hs] by iterating through the list of targets in the session.

* Module Summary
  A sort of header for each module that specifies the module name, textual imports, and other data.
  Stored as part of the module graph. Created during make by [summariseModule](https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/main/GhcMake.hs#L2263).

* Ghc "Make" mode
  The process of compiling modules to some backend language.

* Downsweep
  Recursively walking through the import list of the target modules to analyze dependendencies and create
  a **Module Graph** containing **Module Summaries**

* Upsweep
  The actual compilation process, walking through the topologically sorted module graph and compiling modules
  as necessary. The **Home Package Table** is computed as a result of this process. 

* Home Package Table

* The Driver Pipeline
  Generates intermediate files during compilation through a series of phases. See [Pipeline].

* Compilation Entry Points
  [`compileOne`](https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/main/DriverPipeline.hs#L114) is
  the compilation entry point for GhcMake (called from upsweep_mod). See [Pipeline].

* HSC Main
  The driver pipeline eventually calls hscIncrementalFrontend to parse, rename, and typecheck the target modules.
  This module contains hscTypecheck, what takes an optional parsed module, and then walks it through the renaming/typechecking phases.
  See `hscTypecheck` for the real business in [HscMain.hs]. 
 
* HSC Monad 
  The inner monad for compiling haskell sources.

### References

1. https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/hsc-main  
2. http://hackage.haskell.org/package/ghc-8.6.5  
3. https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#equality-constraints-coercible-and-the-kind-constraint

[GhcMake.hs]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/main/GhcMake.hs#L121
[GhcMonad.hs]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/main/GhcMonad.hs
[Pipeline]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/pipeline<Paste>
[HscMain.hs]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/main/HscMain.hs#L414
