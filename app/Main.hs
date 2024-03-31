{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State.Strict
  ( MonadIO (..),
    MonadState (get),
    StateT (runStateT),
    modify,
  )
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
import GHC.Core (Bind (NonRec), Expr (..))
import GHC.Core.Ppr
  ( pprCoreAlt,
    pprCoreBinding,
    pprCoreBindings,
    pprOptCo,
    pprParendExpr,
  )
import GHC.Driver.Types (ModGuts (mg_binds))
import GHC.Paths (libdir)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString),
  )
import GHC.Types.Var (Var (varName, varType), isId, isTyVar, isTcTyVar, isLocalId, isGlobalId, isLocalVar)
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr)
import SimplifiedCoreAST.SimplifiedCoreAST (AltConS (..), AltS (..), BindS (..), ExpressionS (..), LiteralS (..))
import SimplifiedCoreAST.SimplifiedCoreASTConverter (simplifyBindings)
import SimplifiedCoreAST.SimplifiedCoreASTPrinter (printSimplifiedCoreAST)
import SimplifiedCoreAST.SimplifiedCoreASTReducer (printStepByStepReduction)
import Utils (printAst, showOutputable)

import OriginalCoreAST.CoreStepperPrinter (printCoreStepByStepReductionForBinding, printCoreStepByStepReductionForEveryBinding)
import Options.Applicative
    ( fullDesc,
      header,
      help,
      info,
      infoOption,
      long,
      metavar,
      progDesc,
      strOption,
      value,
      execParser,
      helper,
      Parser,
      ParserInfo )

data Opts = Opts
  { filePath :: !String,
    moduleName :: !String
  }

main :: IO ((), StepState)
main = do
  opts <- execParser optsParser
  runStepper (filePath opts) (moduleName opts)
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> versionOption <*> programOptions)
        ( fullDesc <> progDesc "Haskell Substitution Stepper"
            <> header
              "a stepper for Haskell Core"
        )
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.0" (long "version" <> help "Show version")
    programOptions :: Parser Opts
    programOptions =
      Opts
        <$> strOption
          ( long "file" <> metavar "VALUE" <> value "src/Source2.hs"
              <> help "path to the haskell file that should be read"
          )
        <*> strOption
          ( long "module" <> metavar "VALUE" <> value "Source2"
              <> help "name of the module inside the haskell file"
          )

runStepper :: String -> String -> IO ((), StepState)
runStepper filePath moduleName = runGhc (Just libdir) $ do
  dFlags <- getSessionDynFlags
  setSessionDynFlags dFlags {hscTarget = HscNothing}

  target <- guessTarget filePath Nothing
  addTarget target
  res <- load LoadAllTargets
  case res of
    Succeeded -> liftIO $ putStrLn "successfully loaded targets"
    Failed -> liftIO $ putStrLn "failed to load targets"

  let modName = mkModuleName moduleName
  modSum <- getModSummary modName

  psmod <- parseModule modSum
  tcmod <- typecheckModule psmod
  dsmod <- desugarModule tcmod

  let parserAst = pm_parsed_source psmod
      tcAst = tm_typechecked_source tcmod

      coreModule = dm_core_module dsmod
      coreAst = mg_binds coreModule
  -- coreReaderEnv = mg_rdr_env coreModule
  -- coreTyCons = mg_tcs coreModule
  -- coreClassInsts = mg_insts coreModule
  -- coreFamInsts = mg_fam_insts coreModule
  -- corePatternSyns = mg_patsyns coreModule

  liftIO $ writeFile "parserAst.txt" (printAst parserAst)
  liftIO $ writeFile "tcAst.txt" (printAst tcAst)
  liftIO $ writeFile "coreAST.txt" (printAst coreAst)
  liftIO $ writeFile "coreProgram.txt" (showOutputable coreAst)
  -- liftIO $ writeFile "coreReaderEnv.txt" (printAst coreReaderEnv)
  -- liftIO $ writeFile "coreTyCons.txt" (printAst coreTyCons)
  -- liftIO $ writeFile "coreClassInsts.txt" (printAst coreClassInsts)
  -- liftIO $ writeFile "coreFamInsts.txt" (showOutputable coreFamInsts)
  -- liftIO $ writeFile "corePatternSyns.txt" (printAst corePatternSyns)

  liftIO $ putStrLn "\n*****Pretty Printed Core Bindings:******"
  liftIO (putStrLn (showOutputable (pprCoreBindings coreAst)))

  liftIO $ putStrLn "\n*****Example Original Core AST Reduction(s)*****"
  --show reduction for every binding in the file
  liftIO $ printCoreStepByStepReductionForEveryBinding coreAst

  liftIO $ putStrLn "\n*****Example Stepping*****"
  let addAst = extract coreAst
  runStateT (step addAst) initStepState

extract :: [Bind a] -> Expr a
extract prog =
  let (main : add : rest) = prog
   in case add of
        NonRec j exp -> exp

step :: (OutputableBndr b, MonadState StepState m, MonadIO m) => Expr b -> m ()
step (Var id) = do
  printDepth
  lPrint ("Var", showOutputable $ varName id, showOutputable $ varType id, show (isId id), show (isTyVar id), show (isTcTyVar id), show (isLocalId id), show (isGlobalId id), show (isLocalVar id))
step (Lit lit) = do
  printDepth
  case lit of
    LitChar c -> lPrint ("Char ", c)
    LitNumber t v -> lPrint ("Number ", v)
    LitString bs -> lPrint ("String ", bs)
    LitFloat f -> lPrint ("Float ", f)
    LitDouble d -> lPrint ("Double ", d)
step (App exp arg) = do
  printDepth
  lPutStr "App "
  lOutput $ pprParendExpr exp
  incDepth
  step exp
  step arg
step (Lam b exp) = do
  printDepth
  lPutStr "Lam "
  lOutput' b
  lOutput $ pprParendExpr exp
  incDepth
  step exp
step (Let bind exp) = do
  printDepth
  lPutStr "Let "
  lOutput $ pprCoreBinding bind
  lOutput $ pprParendExpr exp
  incDepth
  step exp
step (Case exp b t alts) = do
  printDepth
  lPutStr "Case "
  lOutput $ pprParendExpr exp
  lOutput' b
  lOutput' $ ppr t
  lOutput $ map pprCoreAlt alts
  incDepth
  step exp
step (Cast exp coer) = do
  printDepth
  lPutStr "Cast "
  lOutput $ pprParendExpr exp
  lOutput $ pprOptCo coer
  incDepth
  step exp
step (Tick tid exp) = do
  printDepth
  lPutStr "Tick "
  lOutput $ pprParendExpr exp
  incDepth
  step exp
step (Type t) = do
  printDepth
  lPutStr "Type "
  lOutput $ ppr t
step (Coercion coer) = do
  printDepth
  lPutStr "Coer "
  lOutput $ pprOptCo coer

-- lifted version of common functions used in the stepper
lOutput :: (MonadState StepState m, MonadIO m, Outputable a) => a -> m ()
lOutput = liftIO . putStrLn . showOutputable

lOutput' :: (MonadState StepState m, MonadIO m, Outputable a) => a -> m ()
lOutput' = liftIO . putStr . showOutputable

lPrint :: (MonadState StepState m, MonadIO m, Show a) => a -> m ()
lPrint = liftIO . print

lPutStr :: (MonadState StepState m, MonadIO m) => String -> m ()
lPutStr = liftIO . putStr

-- state of the stepper
data StepState = StepState {depth :: Integer} deriving (Show)

initStepState :: StepState
initStepState = StepState 0

incDepth :: (MonadState StepState m, MonadIO m) => m ()
incDepth = modify $ \s -> StepState {depth = depth s + 1}

printDepth :: (MonadState StepState m, MonadIO m) => m ()
printDepth = do
  s <- get
  lPutStr (replicate (fromIntegral (depth s)) '=')
