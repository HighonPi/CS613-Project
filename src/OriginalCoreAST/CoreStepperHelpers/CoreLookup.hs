module OriginalCoreAST.CoreStepperHelpers.CoreLookup(findMatchingPattern, tryFindBinding, tryFindBindingForString)
where

import Data.Maybe ()

import OriginalCoreAST.CoreStepperHelpers.CoreTransformator(deepReplaceVarWithinExpression, deepReplaceMultipleVarWithinExpression, convertToMultiArgumentFunction)
import OriginalCoreAST.CoreInformationExtractorFunctions (varToString, nameToString, removeTypeInformation, varNameEqualsString, varsHaveTheSameName, varsHaveTheSameType, varExpressionToString, isTypeInformation)
import OriginalCoreAST.CoreTypeClassInstances()
import OriginalCoreAST.CoreTypeDefinitions (Binding, FunctionName, FunctionReference,)
import Data.List ()
import GHC.Plugins
  ( Alt,
    AltCon (DEFAULT, DataAlt, LitAlt),
    CoreExpr,
    Expr (App, Lit, Var),
    Var,
    dataConName,
    trace,
  )

import Utils (showOutputable)

tryFindBinding :: Var -> [Binding] -> Maybe (Expr Var)
tryFindBinding key [] = Nothing
tryFindBinding key ((var, exp):xs) = if ((==) (varToString var) (varToString key))
                                                    then Just (exp)
                                                    else tryFindBinding key xs

-- | tries to find a binding inside the list of bindings for a given function name
tryFindBindingForString :: String -> [Binding] -> Maybe Binding
tryFindBindingForString functionName [] = Nothing
tryFindBindingForString functionName ((var, exp):xs) = if ((==) (varToString var) functionName) 
                                                                    then Just (var, exp)
                                                                    else tryFindBindingForString functionName xs

findMatchingPattern :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingPattern expression [] = Nothing 
findMatchingPattern _ ((DEFAULT, _, expression):_) = Just expression
findMatchingPattern (Var name) (((DataAlt dataCon), _, expression):xs) = if ((==) (varToString name) (showOutputable dataCon))
                                                                                then Just expression
                                                                                else (findMatchingPattern (Var name) xs)
findMatchingPattern (Lit literal) (((LitAlt patternLiteral), _, expression):xs) = if ((==) literal patternLiteral) --can we compare two literals like this?
                                                                                             then Just expression
                                                                                             else (findMatchingPattern (Lit literal) xs)
findMatchingPattern (App expr argument) (((DataAlt patternConstructorName), boundNames, expression):xs) = do
    let (function, arguments) = convertToMultiArgumentFunction (App expr argument)
    if ((==) (varExpressionToString function) (showOutputable patternConstructorName))
        then Just (deepReplaceMultipleVarWithinExpression boundNames (filter (not . isTypeInformation) arguments) expression)
        else (findMatchingPattern (App expr argument) xs) 
findMatchingPattern expression (x:xs) = findMatchingPattern expression xs
