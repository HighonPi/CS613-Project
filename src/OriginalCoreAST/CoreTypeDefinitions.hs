module OriginalCoreAST.CoreTypeDefinitions where

import Data.List (intercalate)
import GHC.Plugins (CoreExpr, Expr, Var)
import Utils (showOutputable)

type Binding = (Var, Expr Var)

type Reducer = (Expr Var -> Maybe (Expr Var))

type StepResult = (ReductionStepDescription, Expr Var, [Binding])

type Argument = Expr Var

type Function = Expr Var

type FunctionReference = Var

type FunctionName = String

data ReductionSuccessfulFlag
  = Success
  | NoReductionRule
  | StoppedToPreventInfiniteLoop

data ReductionStepDescription
  = DeltaReductionStep FunctionReference
  | ApplicationStep CoreExpr
  | EvaluationStep FunctionReference
  | CaseExpressionStep
  | PatternMatchStep
  | ApplicationExpressionStep
  | StrictApplicationArgumentStep --not real Core behaviour but used for unsteppable functions
  | ConstructorArgumentReductionForVisualization --not real Core behaviour but used for Visualization, for example to reduce expressions like (Maybe (1 + 1))
  | NestedReduction [ReductionStepDescription]

instance Show ReductionStepDescription where
  show (DeltaReductionStep var) = "Replace '" ++ showOutputable var ++ "' with definition"
  show (ApplicationStep _) = "Lamda Application"
  show (EvaluationStep var) = "Evaluate unsteppable function/operator " ++ showOutputable var
  show CaseExpressionStep = "Reduce Case Expression"
  show PatternMatchStep = "Replace with matching pattern"
  show ApplicationExpressionStep = "Reduce function of application"
  show StrictApplicationArgumentStep = "(Strict) reduce application argument"
  show (NestedReduction descriptions) = intercalate " -> " (map show descriptions)
  show (ConstructorArgumentReductionForVisualization) = "Reduction complete - reduce constructor arguments for better visualization"