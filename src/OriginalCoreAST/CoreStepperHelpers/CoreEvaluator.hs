module OriginalCoreAST.CoreStepperHelpers.CoreEvaluator(evaluateFunctionWithArguments)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe ()
import GHC.Core (Expr (..))
import GHC.Types.Literal ( Literal (..))
import GHC.Types.Var (Var)

import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced)


evaluateFunctionWithArguments :: Expr Var -> [Expr Var] -> Maybe (Expr Var)
-- evaluateFunctionWithArguments (Var functionOrOperatorName) _ = error ("called evaluateFunctionWithArguments for function: " ++ (varToString functionOrOperatorName))
evaluateFunctionWithArguments (Var functionOrOperatorName) arguments = do
    evaluateUnsteppableFunctionWithArguments (varToString functionOrOperatorName) (filter (not.isTypeInformation) arguments) --Precondition: function must be in the form of "var". This is already checked by the function which is calling this function.
evaluateFunctionWithArguments _ _ = error "function-expression has to be a 'Var'"

evaluateUnsteppableFunctionWithArguments :: String -> [Expr Var] -> Maybe (Expr Var)
evaluateUnsteppableFunctionWithArguments "+" [x, y] = Just ((+) x y)
evaluateUnsteppableFunctionWithArguments "-" [x, y] = Just ((-) x y)
evaluateUnsteppableFunctionWithArguments "*" [x, y] = Just ((*) x y)
evaluateUnsteppableFunctionWithArguments "/" [x, y] = Just ((/) x y)
evaluateUnsteppableFunctionWithArguments "recip" [x] = Just (recip x)
evaluateUnsteppableFunctionWithArguments "signum" [x] = Just (signum x)
evaluateUnsteppableFunctionWithArguments "abs" [x] = Just (abs x)
evaluateUnsteppableFunctionWithArguments "/=" [x, y] = Just (boolToCoreExpression ((/=) x y))
evaluateUnsteppableFunctionWithArguments "==" [x, y] = Just (boolToCoreExpression ((==) x y))
evaluateUnsteppableFunctionWithArguments "<" [x, y] = Just (boolToCoreExpression ((<) x y))
evaluateUnsteppableFunctionWithArguments ">" [x, y] = Just (boolToCoreExpression ((>) x y))
evaluateUnsteppableFunctionWithArguments ">=" [x, y] = Just (boolToCoreExpression ((>=) x y))
evaluateUnsteppableFunctionWithArguments "<=" [x, y] = Just (boolToCoreExpression ((<=) x y))
evaluateUnsteppableFunctionWithArguments "min" [x, y] = Just (min x y)
evaluateUnsteppableFunctionWithArguments "max" [x, y] = Just (max x y)
evaluateUnsteppableFunctionWithArguments "succ" [x] = Just ((+) x 1)
evaluateUnsteppableFunctionWithArguments "pred" [x] = Just ((-) x 1)
evaluateUnsteppableFunctionWithArguments "negate" [Lit (LitNumber _ x)] = Just (integerToCoreExpression (negate x)) --example of an arbitrary function from the prelude. note how the arguments must have the right type and the result is converted back into an expression
evaluateUnsteppableFunctionWithArguments "unpackCString#" [x] = Just x
evaluateUnsteppableFunctionWithArguments name _ = Nothing --function not supported
--toDo: Implement more operators and functions