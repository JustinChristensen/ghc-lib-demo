# GHC Compiler Pipeline Demonstration

```
-- The pipeline, according to the wiki
-- 1. M.hs          -> Parse        -> HsSyn RdrName
-- 2. HsSyn RdrName -> Rename       -> HsSyn Name
-- 3. HsSyn Name    -> Typecheck    -> HsSyn Id
-- 4. HsSyn Id      -> Desugar      -> CoreExpr
-- 5. CoreExpr      -> Simplify[N]  -> CoreExpr
-- 6. CoreExpr      -> CoreTidy     -> CoreExpr
-- 7. CoreExpr      -> CorePrep     -> CoreExpr
-- 8. CoreExpr      -> To STG       -> STG
-- 9. STG           -> Codegen      -> Cmm
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

* Module Summary (ModSummary)
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

* HomePackageTable
  A finite map of unique module names to HomeModInfo that gets populated as compilation proceeds.

* HomeModInfo

* The Driver Pipeline
  Generates intermediate files during compilation through a series of phases. See [Pipeline].

* Compilation Entry Points
  [`compileOne`](https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/main/DriverPipeline.hs#L114) is
  the compilation entry point for GhcMake (called from upsweep_mod). See [Pipeline].

* HSC Main
  The driver pipeline eventually calls hscIncrementalFrontend to parse, rename, and typecheck the target modules.
  This module contains hscTypecheck, what takes an optional parsed module, and then walks it through the renaming/typechecking phases.
  See `hscTypecheck` for the real business in [HscMain.hs]. 
  After typechecking succeeds, calls `finish` to proceed with desugaring to core and running simplification passes.
 
* HSC Monad 
  The inner monad for compiling haskell sources.

* OccName
  Classified identifier, but not yet qualified. Qualification occurs during the renaming phase. As a RdrName, they're either
  Unqualified, Qualified, or an Original (defining) name.

* Renaming
  The process of turning name occurrences into fully qualified names. See [RnNames.hs]. The typechecker drives this process.
  See also [HsExtension.hs] for the different types of names used by each phase of the compiler. See [lookup_ie] for an example
  of this on import declarations. Also see [tcRnImports] for an example of where the typechecker drives this.

  `tcRnModule` has a flag parameter for whether or not to [retain the renamed declarations](https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/typecheck/TcRnTypes.hs#L615) in the TcGblEnv.

* HSC Env / Session
  Context data for the compiler.

* TcGblEnv 
  The global environment for the typechecker. Contains a **GlobalRdrEnv**. Returned as the result of incremental compilation.

* GlobalRdrEnv
  

### References

1. https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/hsc-main  
2. http://hackage.haskell.org/package/ghc-8.6.5  
3. https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#equality-constraints-coercible-and-the-kind-constraint

[GhcMake.hs]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/main/GhcMake.hs#L121
[GhcMonad.hs]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/main/GhcMonad.hs
[Pipeline]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/pipeline<Paste>
[HscMain.hs]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/main/HscMain.hs#L414
[RnNames.hs]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/rename/RnNames.hs
[HsExtension.hs]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/hsSyn/HsExtension.hs#L81
[lookup_ie]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/rename/RnNames.hs#L946
[tcRnImports]: https://gitlab.haskell.org/ghc/ghc/blob/ghc-8.6.5-release/compiler/typecheck/TcRnDriver.hs#L315
