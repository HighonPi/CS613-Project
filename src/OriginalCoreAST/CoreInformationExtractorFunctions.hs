module OriginalCoreAST.CoreInformationExtractorFunctions
    (   varExpressionToString, 
        varToString, 
        nameToString, 
        coreLiteralToFractional, 
        isInHeadNormalForm, 
        isTypeInformation, 
        canBeReduced,
        convertToMultiArgumentLamda,
        getIndividualElementsOfList,
        getIndividualElementsOfTuple,
        isBoolVar,
        isList,
        isOperator,
        isPrimitiveTypeConstructorApp,
        isTuple,
        removeTypeInformation,
        removeTypeVars,
        showOperatorWithoutBrackets,
        varToSimpleString
    )
where

import GHC.Core (Expr (..))
import GHC.Types.Literal(Literal (..), mkLitInt64, mkLitString)
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import GHC.Types.Name(nameUnique, Name, mkSystemVarName, mkSysTvName, mkSystemName, pprNameUnqualified, nameStableString)
import GHC.Core.Utils (exprIsHNF)
import GHC.Plugins 
    (
        CoreExpr,
        getOccString,
        collectArgs,
        isTyVar
    )

import Data.List (isPrefixOf, isSuffixOf)
import Utils (showOutputable, splitList)

varExpressionToString :: Expr Var -> String
varExpressionToString (Var var) = varToString var
varExpressionToString _ = error "Expression is not a variable"

varToString :: Var -> String
varToString var =
    if isBoolVar (Var var)
        then varToSimpleString var
    else showOutputable var

-- | converts a Var into a printable String
--  showOutputable results in "True_XO"
--  This function results in "True"
varToSimpleString :: Var -> String
varToSimpleString var = nameToString (varName var)

nameToString :: Name -> String
nameToString = getOccString

coreLiteralToFractional :: Fractional a => Literal -> a
coreLiteralToFractional (LitFloat value) = fromRational value
coreLiteralToFractional (LitDouble value) = fromRational value

isInHeadNormalForm :: Expr Var -> Bool
isInHeadNormalForm = exprIsHNF

isTypeInformation :: Expr Var -> Bool
isTypeInformation (Type _) = True
isTypeInformation (Var name) = "$" `isPrefixOf` (varToString name) || isTyVar name
isTypeInformation x = False

-- | The "canBeReducedFunction" checks if a Core expression can be reduced.
canBeReduced exp
  | isTypeInformation exp = False
  | isBoolVar exp = False
  | otherwise = not (exprIsHNF exp)

isBoolVar :: Expr Var -> Bool
isBoolVar (Var x) = or [((==) (varToSimpleString x) "True"), ((==) (varToSimpleString x) "False")]
isBoolVar _ = False

convertToMultiArgumentLamda :: Expr b -> ([b], Expr b)
convertToMultiArgumentLamda (Lam bind expr) = do
  let (nestedBindings, nestedExpression) = convertToMultiArgumentLamda expr
  (bind : nestedBindings, nestedExpression)
convertToMultiArgumentLamda expr = ([], expr)

-- | takes a list represented as a Core expression and returns all the elements as a "real" list
getIndividualElementsOfList :: Expr b -> [Expr b]
getIndividualElementsOfList expr
  | isEmptyList expr = []
  | isList expr = do
    let (constructor, elements) = collectArgs expr
    if length elements /= 3
      then error ("unexpected number of arguments to cons operator: " ++ show (length elements))
      else do
        let [ty, first, nestedList] = take 3 elements
        [ty, first] ++ getIndividualElementsOfList nestedList
  | otherwise = [expr]

-- | checks if a (nested) application is the instanciation of a type with a specific constructor name, provided as String
isConstructorApplicationOfType :: Expr b -> String -> Bool
isConstructorApplicationOfType (App expr arg) name = do
  let (function, arguments) = collectArgs (App expr arg)
  case function of
    (Var var) -> (==) (varToString var) name
    _ -> False
isConstructorApplicationOfType _ _ = False

-- | checks if a Core expression represents an empty list
isEmptyList :: Expr b -> Bool
isEmptyList expr = isConstructorApplicationOfType expr "[]"

-- | checks if a Core expression represents a non-empty list
isNonEmptyList :: Expr b -> Bool
isNonEmptyList (App expr arg) = isConstructorApplicationOfType (App expr arg) ":" && isList arg
isNonEmptyList _ = False

-- | checks if a Core expression represents a list
isList :: Expr b -> Bool
isList expr = (||) (isNonEmptyList expr) (isEmptyList expr)

-- | takes a tuple represented as a Core expression and returns all the elements as a list
getIndividualElementsOfTuple :: Expr b -> [Expr b]
getIndividualElementsOfTuple expr
  | isEmptyTuple expr = []
  | isTuple expr = do
    let (constructor, elements) = collectArgs expr
    let values = snd (splitList elements)
    values
  | otherwise = error "expression is not a tuple"

-- | checks if a Core expression represents a empty tuple
isEmptyTuple :: Expr b -> Bool
isEmptyTuple (Var var) = varToString var == "()"
isEmptyTuple _ = False

-- | checks if a Core expression represents a non-empty tuple
isNonEmptyTuple :: Expr b -> Bool
isNonEmptyTuple (App expr arg) = do
  let (function, arguments) = collectArgs (App expr arg)
  case function of
    (Var var) -> (("(" `isPrefixOf` constructorName) && (")" `isSuffixOf` constructorName)) && (',' `elem` constructorName)
      where
        constructorName = varToString var
    _ -> False
isNonEmptyTuple _ = False

-- | checks if a Core expression represents a tuple
isTuple :: Expr b -> Bool
isTuple expr = (||) (isNonEmptyTuple expr) (isEmptyTuple expr)

-- | checks if an expression is an operator function
--  operator functions would be (+) (-) and other functions with brackets
isOperator :: CoreExpr -> Bool
isOperator (Var var) = ("(" `isPrefixOf` expressionString) && (")" `isSuffixOf` expressionString)
  where
    expressionString = showOutputable (Var var :: CoreExpr)
isOperator _ = False

-- | checks if an expression is an application of a primitive type constructor. For example
--  "I# 3" or "C# 'a'" would be such primitive type constructor applications
isPrimitiveTypeConstructorApp :: Expr b -> Bool
isPrimitiveTypeConstructorApp (App (Var var) (Lit literal)) = isPrimitiveTypeConstructorName (varName var)
isPrimitiveTypeConstructorApp _ = False

-- | checks if a function name stands for a primitive type constructor like "I#" or "C#".
isPrimitiveTypeConstructorName :: Name -> Bool
isPrimitiveTypeConstructorName name = "#" `isSuffixOf` (nameToString name)

-- | takes a list of expressions and returns all expressions that are not type information.
--  this is useful for function evaluation where the type information is not needed
removeTypeInformation :: [Expr Var] -> [Expr Var]
removeTypeInformation list = filter (not . isTypeInformation) list

-- | takes a list of vars and returns all vars that are not type information.
removeTypeVars :: [Var] -> [Var]
removeTypeVars list = filter (not . isTyVar) list

-- | gives the printable representation for an operator without brackets
showOperatorWithoutBrackets :: CoreExpr -> String
showOperatorWithoutBrackets (Var var) = varToString var
showOperatorWithoutBrackets _ = error "is not an operator"