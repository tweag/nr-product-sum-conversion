module Main where

import GHC.Driver.Backend

preReify :: Show a => [(String, Backend)] -> (String, Backend -> a) -> IO ()
reify :: Show a => String -> (Backend -> a) -> IO ()

reify = preReify namedBackends

preReify backends (fname, f) = do
  mapM_ clause backends
  putStrLn ""
 where clause (bname, backend) =
           putStrLn $ unwords [fname, bname, "=", show (f backend)]

namedBackends :: [(String, Backend)]
namedBackends = [ ("NCG", ncgBackend)
                , ("LLVM", llvmBackend)
                , ("ViaC", viaCBackend)
                , ("Interpreter", interpreterBackend)
                , ("NoBackend", noBackend)
                ]

predicates :: [(String, Backend -> Bool)]


main :: IO ()
main =  do
    reify "backendDescription" backendDescription -- String
    reify "backendWritesFiles" backendWritesFiles -- !Bool
    reify "backendPipelineOutput" backendPipelineOutput -- PipelineOutput
    reify "backendCanReuseLoadedCode" backendCanReuseLoadedCode -- Bool
    reify "backendGeneratesCode" backendGeneratesCode -- !Bool
    reify "backendSupportsInterfaceWriting" backendSupportsInterfaceWriting -- !Bool
    reify "backendRespectsSpecialise" backendRespectsSpecialise -- !Bool
    reify "backendWantsGlobalBindings" backendWantsGlobalBindings -- !Bool
    reify "backendHasNativeSwitch" backendHasNativeSwitch -- !Bool
    reify "backendPrimitiveImplementation" backendPrimitiveImplementation -- !PrimitiveImplementation
    reify "backendSimdValidity" backendSimdValidity -- Validity' String
    reify "backendSupportsEmbeddedBlobs" backendSupportsEmbeddedBlobs -- !Bool
    reify "backendNeedsPlatformNcgSupport" backendNeedsPlatformNcgSupport -- !Bool
    reify "backendSupportsUnsplitProcPoints" backendSupportsUnsplitProcPoints -- !Bool
    reify "backendSwappableWithViaC" backendSwappableWithViaC -- !Bool
    reify "backendUnregisterisedAbiOnly" backendUnregisterisedAbiOnly -- !Bool
    reify "backendGeneratesHc" backendGeneratesHc -- !Bool
    reify "backendSptIsDynamic" backendSptIsDynamic -- !Bool
    reify "backendWantsBreakpointTicks" backendWantsBreakpointTicks -- !Bool
    reify "backendForcesOptimization0" backendForcesOptimization0 -- !Bool
    reify "backendNeedsFullWays" backendNeedsFullWays -- !Bool
    reify "backendSpecialModuleSource" backendSpecialModuleSource -- Bool -> Maybe String
    reify "backendSupportsHpc" backendSupportsHpc -- !Bool
    reify "backendSupportsCImport" backendSupportsCImport -- !Bool
    reify "backendSupportsCExport" backendSupportsCExport -- !Bool
    reify "backendAssemblerProg" backendAssemblerProg -- DefunctionalizedAssemblerProg
    reify "backendAssemblerInfoGetter" backendAssemblerInfoGetter -- DefunctionalizedAssemblerInfoGetter
    reify "backendCDefs" backendCDefs -- DefunctionalizedCDefs
    reify "backendCodeOutput" backendCodeOutput -- DefunctionalizedCodeOutput
    reify "backendPostHscPipeline" backendPostHscPipeline -- DefunctionalizedPostHscPipeline
    reify "backendNormalSuccessorPhase" backendNormalSuccessorPhase -- Phase
    reify "backendName" backendName -- Maybe BackendName


mapM_ re failures
