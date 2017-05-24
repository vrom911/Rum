module Compiler.Rum.Compiler.JIT where

import           Control.Monad.Except       (runExceptT)
import           Foreign.Ptr                (FunPtr, castFunPtr )

import qualified LLVM.AST as AST            (Module, Name(..))
import           LLVM.Context               (Context, withContext)
import qualified LLVM.ExecutionEngine as EE (MCJIT, getFunction, withModuleInEngine, withMCJIT)
import           LLVM.Module as Mod         (moduleAST, moduleLLVMAssembly, withModuleFromAST)
import           LLVM.PassManager           (PassSetSpec(..), defaultCuratedPassSetSpec, runPassManager, withPassManager)

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Nothing  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mod =
    withContext $ \context ->
        jit context $ \executionEngine ->
            runExceptT $
            withModuleFromAST context mod $ \m ->
                withPassManager passes $ \pm -> do
                    () <$ runPassManager pm m
                    optmod <- moduleAST m
                    s <- moduleLLVMAssembly m
                    putStrLn s
                    EE.withModuleInEngine executionEngine m $ \ee -> do
                        mainfn <- EE.getFunction ee (AST.Name "main")
                        case mainfn of
                            Just fn -> do
                                res <- run fn
                                putStrLn $ "Evaluated to: " ++ show res
                            Nothing -> return ()
                    return optmod