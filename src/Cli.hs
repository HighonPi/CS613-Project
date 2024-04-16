{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Cli (runCli, dispatch) where

import Compiler (compileToCore, getCoreProgram, writeDump)
import Options.Generic
  ( Generic,
    Modifiers (shortNameModifier),
    ParseRecord (..),
    Text,
    Unwrapped,
    Wrapped,
    defaultModifiers,
    firstLetter,
    parseRecordWithModifiers,
    unwrapRecord,
    type (:::),
    type (<?>),
  )
import OriginalCoreAST.CoreStepperPrinter
  ( printCoreStepByStepReductionForEveryBinding,
  )
import Utils (listTopLevelFunctions, printCore)
import Prelude hiding (FilePath)
import qualified Prelude as P (FilePath)
import System.FilePath (takeDirectory, combine, takeFileName, joinPath)
import System.Directory ( setCurrentDirectory, doesFileExist )
import System.Environment (withArgs)

type FilePath = P.FilePath <?> "The Haskell source file used as input to the tool"

type FunctionName = Maybe String <?> "Top level function to step through"

toolDescription :: Text
toolDescription = "A Tool for Visualizing Reductions in Haskell"

data InputCommand w
  = Step
      { path :: w ::: FilePath,
        function :: w ::: FunctionName,
        verbose :: w ::: Bool <?> "Enable verbose output if set"
      }
  | List
      { path :: w ::: FilePath
      }
  deriving (Generic)

instance ParseRecord (InputCommand Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (InputCommand Unwrapped)

modifiers :: Modifiers
modifiers = defaultModifiers {shortNameModifier = firstLetter}

type Invocation = InputCommand Unwrapped

-- runCli :: IO Invocation
-- runCli = unwrapRecord toolDescription

runCli :: String -> IO Invocation
runCli input = withArgs (words input) $ unwrapRecord toolDescription

dispatch :: Invocation -> IO ()
dispatch (Step p f v) = stepF p f v
dispatch (List p) = listF p

listF :: [Char] -> IO ()
listF fp = do
  cr <- compileToCore fp
  listTopLevelFunctions $ getCoreProgram cr

stepF :: [Char] -> Maybe [Char] -> Bool -> IO ()
stepF fp fn isVerbose = do
  cr <- compileToCore fp
  let preludePath = joinPath [(takeDirectory fp), "MiniPrelude.hs"] 
  fileExists <- (doesFileExist preludePath)
  if (not fileExists)
    then do
        error "Place MiniPrelude.hs in the same directory as your test program!"
    else do
        return()
  mpr <- compileToCore preludePath
  printCoreStepByStepReductionForEveryBinding isVerbose fn (getCoreProgram cr) (getCoreProgram mpr)
