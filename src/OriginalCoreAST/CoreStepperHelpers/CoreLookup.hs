module OriginalCoreAST.CoreStepperHelpers.CoreLookup(tryFindBinding, findMatchingPattern)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator(deepReplaceMultipleVarWithinExpression, convertToMultiArgumentFunction)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced)
import Utils (showOutputable)

type Binding = (Var, Expr Var) --for example x = 2 (x is "var" and 2 is "expr")

tryFindBinding :: Var -> [Binding] -> Maybe (Expr Var)
tryFindBinding key [] = Nothing
tryFindBinding key ((var, exp):xs) = if ((==) (varToString var) (varToString key))
                                                    then Just (exp)
                                                    else tryFindBinding key xs

findMatchingPattern :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingPattern expression [] = Nothing 
findMatchingPattern _ ((DEFAULT, _, expression):_) = Just expression
findMatchingPattern (Var name) (((DataAlt dataCon), _, expression):xs) = if ((==) (varToString name) (showOutputable dataCon)) --check: is there a more elegant way than "show outputable"
                                                                                then Just expression
                                                                                else (findMatchingPattern (Var name) xs)
findMatchingPattern (Lit literal) (((LitAlt patternLiteral), _, expression):xs) = if ((==) literal patternLiteral) --can we compare two literals like this?
                                                                                             then Just expression
                                                                                             else (findMatchingPattern (Lit literal) xs)
findMatchingPattern (App expr argument) (((DataAlt patternConstructorName), boundNames, expression):xs) = do
    let (function, arguments) = convertToMultiArgumentFunction (App expr argument)
    if ((==) (varExpressionToString function) (showOutputable patternConstructorName)) --check: is there a more elegant way than "show outputable"
        then Just (deepReplaceMultipleVarWithinExpression boundNames (filter (not.isTypeInformation) arguments) expression)
        else (findMatchingPattern (App expr argument) xs) 
findMatchingPattern expression (x:xs) = findMatchingPattern expression xs
