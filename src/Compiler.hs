{-# LANGUAGE OverloadedStrings #-}

module Compiler (compileToCore, writeDump, getCoreProgram) where

import Control.Monad.State (MonadIO (liftIO), StateT (runStateT))
import Data.Map.Strict (Map, empty, fromList, mapWithKey, singleton, toList)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T (writeFile)
import GHC
  ( DesugaredModule (dm_core_module),
    DynFlags (hscTarget),
    HscTarget (HscNothing),
    LoadHowMuch (LoadAllTargets),
    ParsedModule (pm_parsed_source),
    SuccessFlag (Failed, Succeeded),
    TypecheckedModule (tm_typechecked_source),
    addTarget,
    desugarModule,
    getModSummary,
    getSessionDynFlags,
    guessTarget,
    load,
    mkModuleName,
    parseModule,
    runGhc,
    setSessionDynFlags,
    typecheckModule,
  )
import GHC.Core (Bind (NonRec), CoreProgram, Expr)
import GHC.Core.Ppr (pprCoreBindings)
import GHC.Driver.Types (ModGuts (mg_binds, mg_fam_insts, mg_insts, mg_patsyns, mg_rdr_env, mg_tcs))
import GHC.Paths (libdir)
import OriginalCoreAST.CoreStepperPrinter
  ( printCoreStepByStepReductionForEveryBinding,
  )
import System.Directory
  ( createDirectory,
    doesDirectoryExist,
    removeDirectory,
  )
import System.FilePath (joinPath, takeBaseName)
import Utils (printAst, showOutputable, textAst, textOutputable)

data CompilationResult = CompilationResult
  { coreProgram :: CoreProgram,
    debugInfo :: Map Text Text
  }

getCoreProgram :: CompilationResult -> CoreProgram
getCoreProgram = coreProgram

compileToCore :: FilePath -> IO CompilationResult
compileToCore filePath = runGhc (Just libdir) $ do
  dFlags <- getSessionDynFlags
  setSessionDynFlags dFlags {hscTarget = HscNothing}

  target <- guessTarget filePath Nothing
  addTarget target
  res <- load LoadAllTargets
  case res of
    Succeeded -> liftIO $ putStrLn "successfully loaded targets"
    Failed -> liftIO $ putStrLn "failed to load targets"

  let modName = mkModuleName (takeBaseName filePath)
  modSum <- getModSummary modName

  psmod <- parseModule modSum
  tcmod <- typecheckModule psmod
  dsmod <- desugarModule tcmod

  let parserAst = pm_parsed_source psmod
      tcAst = tm_typechecked_source tcmod
      coreModule = dm_core_module dsmod
      coreAst = mg_binds coreModule
      coreReaderEnv = mg_rdr_env coreModule
      coreTyCons = mg_tcs coreModule
      coreClassInsts = mg_insts coreModule
      coreFamInsts = mg_fam_insts coreModule
      corePatternSyns = mg_patsyns coreModule
      debugInformation =
        fromList
          [ ("parserAst.txt", textAst parserAst),
            ("tcAst.txt", textAst tcAst),
            ("coreAST.txt", textAst coreAst),
            ("coreProgram.txt", textOutputable coreAst),
            ("coreReaderEnv.txt", textAst coreReaderEnv),
            ("coreTyCons.txt", textAst coreTyCons),
            ("coreClassInsts.txt", textAst coreClassInsts),
            ("coreFamInsts.txt", textOutputable coreFamInsts),
            ("corePatternSyns.txt", textAst corePatternSyns)
          ]

  return $ CompilationResult coreAst debugInformation

debugDirectoryPath :: String
debugDirectoryPath = "dump"

writeDump :: CompilationResult -> IO ()
writeDump cr = do
  debugDirExists <- doesDirectoryExist debugDirectoryPath
  if debugDirExists
    then do
      removeDirectory debugDirectoryPath
      createDirectory debugDirectoryPath
    else do
      createDirectory debugDirectoryPath
  mapM_ (\(k, v) -> T.writeFile (joinPath [debugDirectoryPath, unpack k]) v) (toList (debugInfo cr))
