{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Cli (runCli, dispatch) where

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
import Prelude hiding (FilePath)
import qualified Prelude as P (FilePath)
import Compiler (compileToCore, getCoreProgram, writeDump)
import OriginalCoreAST.CoreStepperPrinter (printCoreStepByStepReductionForEveryBinding)
import Utils (printCore, listTopLevelFunctions)

type FilePath = P.FilePath <?> "The Haskell source file used as input to substep"

type FunctionName = Maybe String <?> "Top level function to step through"

type VerbosityLevel = Maybe Integer <?> "Verbosity level between 1 and 3"

subStepDescription :: Text
subStepDescription = "The Haskell Substitution Stepper"

data SubStep w
  = Step
      { path :: w ::: FilePath,
        function :: w ::: FunctionName,
        verbose :: w ::: VerbosityLevel
      }
  | Print
      { path :: w ::: FilePath,
        function :: w ::: FunctionName
      }
  | List
      { path :: w ::: FilePath
      }
  | Dump {
    path :: w ::: FilePath
  }
  deriving (Generic)

instance ParseRecord (SubStep Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (SubStep Unwrapped)

modifiers :: Modifiers
modifiers = defaultModifiers {shortNameModifier = firstLetter}

type Invocation = SubStep Unwrapped

runCli :: IO Invocation
runCli = unwrapRecord subStepDescription

dispatch :: Invocation -> IO ()
dispatch (Step p f v) = stepF p f v
dispatch (Print p f) = printF p f
dispatch (List p) = listF p
dispatch (Dump p) = dumpF p

dumpF :: [Char] -> IO ()
dumpF fp = do
  cr <- compileToCore fp
  writeDump cr

listF :: [Char] -> IO ()
listF fp = do
  cr <- compileToCore fp
  listTopLevelFunctions $ getCoreProgram cr

printF :: [Char] -> Maybe [Char] -> IO ()
printF fp fn = do
  cr <- compileToCore fp
  printCore $ getCoreProgram cr

stepF :: [Char] -> Maybe [Char] -> Maybe Integer -> IO ()
stepF fp fn v = do
  cr <- compileToCore fp
  printCoreStepByStepReductionForEveryBinding (getCoreProgram cr)
