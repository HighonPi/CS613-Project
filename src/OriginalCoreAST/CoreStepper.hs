module OriginalCoreAST.CoreStepper(applyStep, safeReduceToNormalForm) where

import OriginalCoreAST.CoreTypeClassInstances ()

import OriginalCoreAST.CoreInformationExtractorFunctions (canBeReduced, canBeReducedToNormalForm)
import OriginalCoreAST.CoreStepperHelpers.CoreEvaluator (evaluateFunctionWithArguments)
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator (convertFunctionApplicationWithArgumentListToNestedFunctionApplication, deepReplaceVarWithinExpression, convertToMultiArgumentFunction)
import OriginalCoreAST.CoreStepperHelpers.CoreLookup (tryFindBinding, findMatchingPattern)

import OriginalCoreAST.CoreTypeDefinitions
  ( Argument,
    Binding,
    Function,
    FunctionReference,
    ReductionStepDescription
      ( ApplicationExpressionStep,
        ApplicationStep,
        CaseExpressionStep,
        DeltaReductionStep,
        EvaluationStep,
        NestedReduction,
        PatternMatchStep,
        StrictApplicationArgumentStep
      ),
    StepResult,
  )

import Data.List (find)
import Data.Maybe (fromJust, isJust, isNothing)
import GHC.Plugins
  ( Bind (NonRec, Rec),
    CoreExpr,
    Expr (App, Case, Lam, Var),
    trace,
  )
import Utils (showOutputable)

-- | Maximum number of reductions until inferring it's an infinite-loop when reducing from Head Normal form to a Normal form
maximumAmoutOfReductions :: Integer
maximumAmoutOfReductions = 200

-- | takes an expression and applies one single reduction rule (if possible)
applyStep :: [Binding] -> CoreExpr -> Maybe StepResult
applyStep bindings (Var name) = do
  foundBinding <- tryFindBinding name bindings
  return (DeltaReductionStep name, foundBinding, bindings)

applyStep bindings (App expr arg) = do
  applyStepToNestedApplication bindings (App expr arg) --multi-parameter applications are represented as nested applications in Haskell Core

applyStep bindings (Case expression binding caseType alternatives) = do
  if canBeReduced expression
    then do
      (reductionStep, reducedExpression, newBindings) <- applyStep bindings expression
      return (NestedReduction [CaseExpressionStep, reductionStep], Case reducedExpression binding caseType alternatives, newBindings)
    else do
      matchingPattern <- findMatchingPattern expression alternatives
      return (PatternMatchStep, matchingPattern, bindings)

applyStep _ _ = do Nothing

-- | takes an expression containing a (nested) application and applies one single reduction rule.
applyStepToNestedApplication :: [Binding] -> CoreExpr -> Maybe StepResult
applyStepToNestedApplication bindings expr = tryApplyStepToApplication bindings expr

-- | takes an expression containing a (nested) application and applies one single reduction rule (if possible)
tryApplyStepToApplication :: [Binding] -> CoreExpr -> Maybe StepResult
tryApplyStepToApplication bindings expr = do
  let (function, arguments) = convertToMultiArgumentFunction expr
  tryApplyStepToFunctionWithArguments bindings function arguments
  where
    tryApplyStepToFunctionWithArguments :: [Binding] -> Function -> [Argument] -> Maybe StepResult
    tryApplyStepToFunctionWithArguments bindings (Var var) arguments = do
      if isJust (tryFindBinding var bindings)
        then do
          --function or operator can be stepped
          (reductionStep, reducedFunction, newBindings) <- applyStep bindings (Var var)
          return (NestedReduction [ApplicationExpressionStep, reductionStep], convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction arguments, newBindings)
        else do
          --function or operator cannot be stepped
          applyStepToApplicationWithAnUnsteppableFunction bindings var arguments
    tryApplyStepToFunctionWithArguments bindings (Lam lamdaParameter lamdaExpression) arguments = do
      let reducedFunction = deepReplaceVarWithinExpression lamdaParameter (head arguments) lamdaExpression
      Just (ApplicationStep (head arguments), convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments), bindings)
    tryApplyStepToFunctionWithArguments bindings expression arguments = do
      (reductionStep, reducedFunction, newBindings) <- applyStep bindings expression
      return (NestedReduction [ApplicationExpressionStep, reductionStep], convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction arguments, newBindings)

-- | takes an expression containing a (nested) application. If at least one of the arguments to the function can be
--  reduced, one argumen gets reduced. Please note that this is strict behaviour. If all arguments are at least in
--  weak head normal form, the function gets evaluated
applyStepToApplicationWithAnUnsteppableFunction :: [Binding] -> FunctionReference -> [Argument] -> Maybe StepResult
applyStepToApplicationWithAnUnsteppableFunction bindings function arguments = do
  if any canBeReduced arguments --all arguments are reduced, eval function. This is stric behaviour! We have to use strict behaviour here because we are trying to evaluate a function whose definition we do not know. therefore we cannot apply the arguments one after another but have to simplify all arguments before calling the function
    then do
      --reduce one of the arguments
      (reductionStep, simplifiedArguments, newBindings) <- applyStepToOneOfTheArguments bindings [] arguments
      return (NestedReduction [StrictApplicationArgumentStep, reductionStep], convertFunctionApplicationWithArgumentListToNestedFunctionApplication (Var function) simplifiedArguments, newBindings)
    else do
      --finally evaluate the function with the eargerly evaluted arguments
      appliedFunction <- evaluateFunctionWithArguments function arguments (safeReduceToNormalForm bindings)
      return (EvaluationStep function, appliedFunction, bindings)
  where
    applyStepToOneOfTheArguments :: [Binding] -> [Argument] -> [Argument] -> Maybe (ReductionStepDescription, [Argument], [Binding])
    applyStepToOneOfTheArguments bindings alreadyReducedArguments (x : xs) =
      if canBeReduced x
        then do
          (description, reducedArgument, newBindings) <- applyStep bindings x
          return (description, (alreadyReducedArguments ++ [reducedArgument]) ++ xs, newBindings)
        else applyStepToOneOfTheArguments bindings (alreadyReducedArguments ++ [x]) xs
    applyStepToOneOfTheArguments bindings alreadyReducedArguments [] = error "no reducable argument found" --no argument that can be reduced was found. this should not happen because this condition gets checked earlier in the code

-- | takes an expression and reduces it until normal form without showing substeps.
--  Nothing is returned if reduction to normal form is not possible or takes too much steps (prevention of infinite loops)
safeReduceToNormalForm :: [Binding] -> CoreExpr -> Maybe CoreExpr
safeReduceToNormalForm = reduceToNormalFormWithMaximumAmountOfReductions maximumAmoutOfReductions

-- | takes an expression and reduces it until normal form without showing substeps.
--  Limit on reduction steps to prevent infinite loops
reduceToNormalFormWithMaximumAmountOfReductions :: Integer -> [Binding] -> CoreExpr -> Maybe CoreExpr
reduceToNormalFormWithMaximumAmountOfReductions 0 _ _ = Nothing -- Considered infinite loop
reduceToNormalFormWithMaximumAmountOfReductions maximumAmountOfReductionsLeft bindings expression = do
  expressionInHeadNormalForm <- reduceToHeadNormalForm bindings expression
  if canBeReducedToNormalForm expressionInHeadNormalForm
    then do
      let (function, arguments) = convertToMultiArgumentFunction expressionInHeadNormalForm
      let maybeReducedArguments = map (reduceToNormalFormWithMaximumAmountOfReductions (maximumAmountOfReductionsLeft - 1) bindings) arguments
      if any isNothing maybeReducedArguments
        then Nothing
        else Just $ convertFunctionApplicationWithArgumentListToNestedFunctionApplication function (map fromJust maybeReducedArguments)
    else Just expressionInHeadNormalForm

-- | takes an expression and reduces it until head normal form without showing substeps.
reduceToHeadNormalForm :: [Binding] -> CoreExpr -> Maybe CoreExpr
reduceToHeadNormalForm bindings expression
  | canBeReduced expression = do
    let reduction = applyStep bindings expression
    case reduction of
      Just (reductionStepDescription, reducedExpression, newBindings) -> reduceToHeadNormalForm newBindings reducedExpression
      Nothing -> Nothing -- No reduction rule is implemented
  | otherwise = Just expression

-- functionIsUnpackingCString :: Expr Var -> Bool
-- functionIsUnpackingCString (Var functionName) = (==) (varToString functionName) "unpackCString#"