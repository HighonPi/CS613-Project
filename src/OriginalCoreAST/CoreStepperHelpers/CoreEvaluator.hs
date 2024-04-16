module OriginalCoreAST.CoreStepperHelpers.CoreEvaluator(evaluateFunctionWithArguments)
where

import Data.Maybe ()
import GHC.Core (Expr (..))
import GHC.Types.Literal ( Literal (..))
import GHC.Types.Var (Var)

import OriginalCoreAST.CoreMakerFunctions(rationalToCoreExpression, integerToCoreExpression, boolToCoreExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(varToString, removeTypeInformation)
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator(prepareExpressionArgumentForEvaluation)
import OriginalCoreAST.CoreTypeClassInstances()
import OriginalCoreAST.CoreTypeDefinitions (Argument, FunctionName, FunctionReference, Reducer)
import Data.Maybe (fromJust, isJust, isNothing)
import GHC.Plugins (CoreExpr, Expr (Lit, Type))
import Utils (showOutputable)

-- | Evaluate unsteppable reductions
--  This function is used for resolving functions that are unsteppable (for example the + operator)
evaluateFunctionWithArguments :: FunctionReference -> [Argument] -> Reducer -> Maybe CoreExpr
evaluateFunctionWithArguments functionOrOperatorName arguments reducer = do
  if any isNothing reducedMaybeArguments
    then 
        Nothing -- cannot reduce argument of unsteppable function
    else
        -- error ("For function: " ++ (varToString functionOrOperatorName) ++ " evaluated result: " ++ (showOutputable evaluationResults))
        evaluationResults
  where
    argumentsWithoutApplications = map prepareExpressionArgumentForEvaluation arguments
    reducedMaybeArguments = map reducer argumentsWithoutApplications
    reducedArguments = map fromJust reducedMaybeArguments

    evaluationResults = evaluateUnsteppableFunctionWithArguments (varToString functionOrOperatorName) (removeTypeInformation reducedArguments) reducer

    evaluateUnsteppableFunctionWithArguments :: FunctionName -> [Argument] -> Reducer -> Maybe CoreExpr
    evaluateUnsteppableFunctionWithArguments "+" [x, y] _ = Just ((+) x y)
    evaluateUnsteppableFunctionWithArguments "-" [x, y] _ = Just ((-) x y)
    evaluateUnsteppableFunctionWithArguments "*" [x, y] _ = Just ((*) x y)
    evaluateUnsteppableFunctionWithArguments "/" [x, y] _ = Just ((/) x y)
    evaluateUnsteppableFunctionWithArguments "recip" [x] _ = Just (recip x)
    evaluateUnsteppableFunctionWithArguments "signum" [x] _ = Just (signum x)
    evaluateUnsteppableFunctionWithArguments "abs" [x] _ = Just (abs x)
    evaluateUnsteppableFunctionWithArguments "/=" [x, y] _ = Just (boolToCoreExpression ((/=) x y))
    evaluateUnsteppableFunctionWithArguments "==" [x, y] _ = Just (boolToCoreExpression ((==) x y))
    evaluateUnsteppableFunctionWithArguments "<" [x, y] _ = Just (boolToCoreExpression ((<) x y))
    evaluateUnsteppableFunctionWithArguments ">" [x, y] _ = Just (boolToCoreExpression ((>) x y))
    evaluateUnsteppableFunctionWithArguments ">=" [x, y] _ = Just (boolToCoreExpression ((>=) x y))
    evaluateUnsteppableFunctionWithArguments "<=" [x, y] _ = Just (boolToCoreExpression ((<=) x y))
    evaluateUnsteppableFunctionWithArguments "min" [x, y] _ = Just (min x y)
    evaluateUnsteppableFunctionWithArguments "max" [x, y] _ = Just (max x y)
    evaluateUnsteppableFunctionWithArguments "succ" [x] _ = Just ((+) x 1)
    evaluateUnsteppableFunctionWithArguments "pred" [x] _ = Just ((-) x 1)
    evaluateUnsteppableFunctionWithArguments "negate" [Lit (LitNumber _ x)] _ = Just (integerToCoreExpression (negate x)) --example of an arbitrary function from the prelude. note how the arguments must have the right type and the result is converted back into an expression
    evaluateUnsteppableFunctionWithArguments "unpackCString#" [x] _ = Just x
    evaluateUnsteppableFunctionWithArguments name _ _ = Nothing --function not supported