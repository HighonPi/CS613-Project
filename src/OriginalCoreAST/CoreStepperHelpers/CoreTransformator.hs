module OriginalCoreAST.CoreStepperHelpers.CoreTransformator(convertFunctionApplicationWithArgumentListToNestedFunctionApplication, deepReplaceVarWithinExpression, deepReplaceVarWithinAlternative, deepReplaceMultipleVarWithinExpression, convertToMultiArgumentFunction)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)

import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced)

convertFunctionApplicationWithArgumentListToNestedFunctionApplication :: Expr Var -> [Expr Var] -> Expr Var
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression [] = expression
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression arguments = App (convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression (init arguments)) (last arguments)

deepReplaceVarWithinExpression :: Var -> Expr Var -> Expr Var -> Expr Var
deepReplaceVarWithinExpression name replaceExpression (Var varName) = if (==) (varToString varName) (varToString name) then replaceExpression else (Var varName)
deepReplaceVarWithinExpression name replaceExpression (App expression argument) = App (deepReplaceVarWithinExpression name replaceExpression expression) (deepReplaceVarWithinExpression name replaceExpression argument)
deepReplaceVarWithinExpression name replaceExpression (Lam parameter expression) =
  if (varToString parameter) == (varToString name)
    then Lam parameter expression --do nothing, use local lamda parameter with the same name (shadowing)
    else Lam parameter (deepReplaceVarWithinExpression name replaceExpression expression)
deepReplaceVarWithinExpression name replaceExpression (Case expression binding caseType alternatives) = Case (deepReplaceVarWithinExpression name replaceExpression expression) binding caseType (map (deepReplaceVarWithinAlternative name replaceExpression) alternatives)
deepReplaceVarWithinExpression _ _ expression = expression --nothing to replace (ToDo: Let, Cast, Tick, Type, Coercion not implemented yet)

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

