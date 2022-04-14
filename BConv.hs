module Main where

import GHC.Driver.Backend
import GHC.Utils.Error

preReify :: Show a => [(String, Backend)] -> String -> (Backend -> a) -> String -> IO ()
reify :: Show a => String -> (Backend -> a) -> String -> IO ()

reify = preReify namedBackends

preReify backends fname f ty = do
  putStrLn $ unwords [fname, "::", "Backend", "->", ty]
  mapM_ clause backends
  putStrLn ""
 where clause (bname, backend) =
           putStrLn $ unwords [fname, "(Named", bname ++ ")", "=", show (f backend)]

namedBackends, outputtingBackends :: [(String, Backend)]
namedBackends = [ ("NCG", ncgBackend)
                , ("LLVM", llvmBackend)
                , ("ViaC", viaCBackend)
                , ("Interpreter", interpreterBackend)
                , ("NoBackend", noBackend)
                ]

outputtingBackends = [ ("NCG", ncgBackend)
                     , ("LLVM", llvmBackend)
                     , ("ViaC", viaCBackend)
                     ]


newtype SV = SV (Validity' String)
instance Show SV where
  show (SV (NotValid s)) = unwords ["NotValid", "$", "unlines", show (lines s)]
  show (SV IsValid) = "IsValid"

newtype SM = SM (Bool -> Maybe String)
instance Show SM where
    show (SM f) =
             if f True == f False then
                 unwords ["const", showMaybe (f True)]
             else
                 unwords ["\\b -> if b then", show (f True), "else", show (f False)]
      where showMaybe Nothing = "Nothing"
            showMaybe m = "(" ++ show m ++ ")"

main :: IO ()
main =  do
    putStrLn "newtype Backend = Named BackendName"
    putStrLn ""

    reify "backendDescription" backendDescription "String"
    reify "backendWritesFiles" backendWritesFiles "Bool"
    reify "backendPipelineOutput" backendPipelineOutput "PipelineOutput"
    reify "backendCanReuseLoadedCode" backendCanReuseLoadedCode "Bool"
    reify "backendGeneratesCode" backendGeneratesCode "Bool"
    reify "backendSupportsInterfaceWriting" backendSupportsInterfaceWriting "Bool"
    reify "backendRespectsSpecialise" backendRespectsSpecialise "Bool"
    reify "backendWantsGlobalBindings" backendWantsGlobalBindings "Bool"
    reify "backendHasNativeSwitch" backendHasNativeSwitch "Bool"
    reify "backendPrimitiveImplementation" backendPrimitiveImplementation "PrimitiveImplementation"
    reify "backendSimdValidity" (SV . backendSimdValidity) "Validity' String" -- no show
    reify "backendSupportsEmbeddedBlobs" backendSupportsEmbeddedBlobs "Bool"
    reify "backendNeedsPlatformNcgSupport" backendNeedsPlatformNcgSupport "Bool"
    reify "backendSupportsUnsplitProcPoints" backendSupportsUnsplitProcPoints "Bool"
    reify "backendSwappableWithViaC" backendSwappableWithViaC "Bool"
    reify "backendUnregisterisedAbiOnly" backendUnregisterisedAbiOnly "Bool"
    reify "backendGeneratesHc" backendGeneratesHc "Bool"
    reify "backendSptIsDynamic" backendSptIsDynamic "Bool"
    reify "backendWantsBreakpointTicks" backendWantsBreakpointTicks "Bool"
    reify "backendForcesOptimization0" backendForcesOptimization0 "Bool"
    reify "backendNeedsFullWays" backendNeedsFullWays "Bool"
    reify "backendSpecialModuleSource" (SM . backendSpecialModuleSource) "Bool -> Maybe String"
    reify "backendSupportsHpc" backendSupportsHpc "Bool"
    reify "backendSupportsCImport" backendSupportsCImport "Bool"
    reify "backendSupportsCExport" backendSupportsCExport "Bool"
    reify "backendAssemblerProg" backendAssemblerProg "DefunctionalizedAssemblerProg"
    reify "backendAssemblerInfoGetter" backendAssemblerInfoGetter "DefunctionalizedAssemblerInfoGetter"
    reify "backendCDefs" backendCDefs "DefunctionalizedCDefs"
    preReify outputtingBackends "backendCodeOutput" backendCodeOutput "DefunctionalizedCodeOutput"
    putStrLn $ "backendCodeOutput Interpreter = panic " ++ show "backendCodeOutput: interpreterBackend"
    putStrLn $ "backendCodeOutput noBackend = panic " ++ show "backendCodeOutput: noBackend"
    reify "backendPostHscPipeline" backendPostHscPipeline "DefunctionalizedPostHscPipeline"
    reify "backendNormalSuccessorPhase" backendNormalSuccessorPhase "Phase"
    reify "backendName" backendName "Maybe BackendName"
