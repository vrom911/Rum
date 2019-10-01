module Rum.Compiler.JIT
       ( runJit
       ) where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Data.ByteString.Char8 (unpack)
import Foreign.Ptr (FunPtr, castFunPtr)
--import Foreign.C.Types (CInt (..))
import LLVM.Context (Context, withContext)
import LLVM.Module as Mod (moduleAST, moduleLLVMAssembly, withModuleFromAST)
import LLVM.PassManager (PassSetSpec (..), defaultCuratedPassSetSpec, runPassManager,
                         withPassManager)

import qualified LLVM.AST as AST (Module, Name (..))
import qualified LLVM.ExecutionEngine as EE (MCJIT, getFunction, withMCJIT, withModuleInEngine)

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

--foreign import ccall safe "rumRead" rumRead :: CInt


runJit :: AST.Module -> IO AST.Module
runJit llvmMod =
    withContext $ \context ->
        jit context $ \executionEngine ->
            withModuleFromAST context llvmMod $ \m ->
                withPassManager passes $ \pm -> do
                    () <$ runPassManager pm m
                    optmod <- moduleAST m
                    s <- moduleLLVMAssembly m
                    writeFile "local_example.ll" $ unpack s
                    EE.withModuleInEngine executionEngine m $ \ee -> do
                        mainfn <- EE.getFunction ee (AST.Name "main")
                        case mainfn of
                            Just fn -> void $ run fn
--                                putStrLn $ "Evaluated to: " ++ show res
                            Nothing -> return ()
                    pure optmod

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
passes = defaultCuratedPassSetSpec { optLevel = Just 0 }
