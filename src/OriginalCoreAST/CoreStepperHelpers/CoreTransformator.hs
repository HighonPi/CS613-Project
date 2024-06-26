module OriginalCoreAST.CoreStepperHelpers.CoreTransformator (
    convertFunctionApplicationWithArgumentListToNestedFunctionApplication, deepReplaceVarWithinExpression, 
    deepReplaceVarWithinAlternative, deepReplaceMultipleVarWithinExpression, convertToMultiArgumentFunction, prepareExpressionArgumentForEvaluation
    )
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe ()
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)

import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced, isTypeWrapperFunctionName)

import GHC.Plugins (CoreExpr)

convertFunctionApplicationWithArgumentListToNestedFunctionApplication :: Expr Var -> [Expr Var] -> Expr Var
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression [] = expression
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression arguments = App (convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression (init arguments)) (last arguments)

-- | Replace all occurances of a given var inside an expression with another expression
deepReplaceVarWithinExpression :: Var -> CoreExpr -> CoreExpr -> CoreExpr
deepReplaceVarWithinExpression name replaceExpression (Var varName) = 
    if (==) (varToString varName) (varToString name) 
    then replaceExpression 
    else (Var varName)

deepReplaceVarWithinExpression name replaceExpression (App expression argument) = 
    App (deepReplaceVarWithinExpression name replaceExpression expression) (deepReplaceVarWithinExpression name replaceExpression argument)

deepReplaceVarWithinExpression name replaceExpression (Lam parameter expression) =
  if varToString parameter == varToString name
    then Lam parameter expression --do nothing, use local lamda parameter with the same name (shadowing)
    else Lam parameter (deepReplaceVarWithinExpression name replaceExpression expression)

deepReplaceVarWithinExpression name replaceExpression (Case expression binding caseType alternatives) =
  Case (deepReplaceVarWithinExpression name replaceExpression expression) binding caseType (map (deepReplaceVarWithinAlternative name replaceExpression) alternatives)

deepReplaceVarWithinExpression _ _ expression = expression --nothing to replace



deepReplaceVarWithinAlternative :: Var -> Expr Var -> Alt Var -> Alt Var 
deepReplaceVarWithinAlternative name replaceExpression (altCon, localBoundVars, expression) = if (elem (varToString name) (map varToString localBoundVars))
                                                                                                 then (altCon, localBoundVars, expression) --do nothing, use local parameter with the same name (shadowing)
                                                                                                 else (altCon, localBoundVars, (deepReplaceVarWithinExpression name replaceExpression expression))

deepReplaceMultipleVarWithinExpression :: [Var] -> [Expr Var] -> Expr Var -> Expr Var
deepReplaceMultipleVarWithinExpression [] _ expression = expression
deepReplaceMultipleVarWithinExpression _ [] expression = expression
deepReplaceMultipleVarWithinExpression (x:xs) (y:ys) expression = deepReplaceMultipleVarWithinExpression xs ys (deepReplaceVarWithinExpression x y expression)

convertToMultiArgumentFunction :: Expr Var -> (Expr Var, [Expr Var])
convertToMultiArgumentFunction expr = collectArgs expr

-- | Unwrapps the argument from a type wrapper if necessary
prepareExpressionArgumentForEvaluation :: CoreExpr -> CoreExpr
prepareExpressionArgumentForEvaluation (App (Var id) arg) =
  if isTypeWrapperFunctionName (varToString id)
    then arg
    else App (Var id) arg
prepareExpressionArgumentForEvaluation x = x