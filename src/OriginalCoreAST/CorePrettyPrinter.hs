module OriginalCoreAST.CorePrettyPrinter(prettyPrint, toHaskellLikeString)
where

import OriginalCoreAST.CoreStepperHelpers.CoreTransformator
  ( convertToMultiArgumentFunction,
  )

import OriginalCoreAST.CoreInformationExtractorFunctions
  ( convertToMultiArgumentLamda,
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
    varToSimpleString,
  )


-- import GHC.Core (Expr (..))
import OriginalCoreAST.CoreTypeClassInstances ()
import Data.List (intercalate)
import GHC.Core.DataCon ()
-- import GHC.Types.Var (Var)
import GHC.Plugins
  ( Alt,
    AltCon (..),
    CoreExpr,
    Expr (App, Case, Cast, Coercion, Lam, Let, Lit, Tick, Type, Var),
    Type,
    Var,
  )

import Utils (showOutputable, increaseIndentation)

prettyPrint :: Expr Var -> IO()
prettyPrint exp = putStrLn (toHaskellLikeString False exp)

---------------------  

-- | converts a Core Expression to a String in a way that is easier to read and resemmbles the original Haskell syntax
toHaskellLikeString :: Bool -> CoreExpr -> String
toHaskellLikeString removeTypes expr = showLikeHaskellWithIndentation expr
  where
    showLikeHaskellWithIndentation :: CoreExpr -> String
    showLikeHaskellWithIndentation expr = do
      showLikeHaskell True expr
      where
        showLikeHaskell :: Bool -> CoreExpr -> String
        showLikeHaskell _ (Var var) = showVar var
        showLikeHaskell _ (Lit literal) = show literal
        showLikeHaskell _ (App expr arg) | isList (App expr arg) = showCoreList (getIndividualElementsOfList (App expr arg))
        showLikeHaskell _ (App expr arg) | isTuple (App expr arg) = showTuple (getIndividualElementsOfTuple (App expr arg))
        showLikeHaskell isOutermostLevel (App expr arg) = do
          if isPrimitiveTypeConstructorApp (App expr arg)
            then showLikeHaskell isOutermostLevel arg
            else showApplication isOutermostLevel (convertToMultiArgumentFunction (App expr arg))
        showLikeHaskell isOutermostLevel (Lam bind expr) = showLam isOutermostLevel (convertToMultiArgumentLamda (Lam bind expr))
        showLikeHaskell _ (Let bind expr) = "Let is not supported"
        showLikeHaskell _ (Case expression binding caseType alternatives) = showCase expression binding caseType alternatives
        showLikeHaskell isOutermostLevel (Cast expression _) = "Cast is not supported"
        showLikeHaskell isOutermostLevel (Tick _ expression) = "Tick is not supported"
        showLikeHaskell _ (Type ty) = "Type is not supported"
        showLikeHaskell _ (Coercion _) = "Coercion is not supported"


        -- checks if a printed expression expands over multiple lines or one single long line.
        -- this check is used to pretty print expressions and add additional line breaks where useful
        isMultiLineOrLongExpression :: CoreExpr -> Bool
        isMultiLineOrLongExpression expression = do
          let expressionString = showLikeHaskell False expression
          isMultiLineOrLongString expressionString
          where
            isMultiLineOrLongString :: String -> Bool
            isMultiLineOrLongString text = ('\n' `elem` text) || (length text > 100)

        showVar :: Var -> String
        showVar var = do
          if isBoolVar (Var var)
            then varToSimpleString var
            else showOutputable (Var var :: CoreExpr)

        showCase :: CoreExpr -> Var -> Type -> [Alt Var] -> String
        showCase expr binding caseType alternatives = do
          let resortedAlternatives = resortAlternatives [] alternatives --default case has to be the last not the first
          let expressionString = showLikeHaskell True expr
          let alternativesString = intercalate ";\n" (map showAlternative resortedAlternatives)
          if isMultiLineOrLongExpression expr
            then "case\n" ++ increaseIndentation 1 expressionString ++ "\nof {\n" ++ increaseIndentation 1 alternativesString ++ "\n}"
            else "case " ++ expressionString ++ " of {\n" ++ increaseIndentation 1 alternativesString ++ "\n}"
          where
            resortAlternatives :: [Alt Var] -> [Alt Var] -> [Alt Var]
            resortAlternatives previous ((altCon, bindings, expression) : rest) =
              case altCon of
                DEFAULT -> (previous ++ rest) ++ [(altCon, bindings, expression)] --put default case last
                _ -> resortAlternatives (previous ++ [(altCon, bindings, expression)]) rest --search for default case
            resortAlternatives previous [] = previous --there is no default case
            showAlternative :: Alt Var -> String
            showAlternative (con, bindings, expr) = do
              let constructorString = showAlternativConstructor con
              let letBindingsString =
                    if null bindings
                      then ""
                      else unwords (map showVar bindings) ++ " "
              let expressionString = showLikeHaskell False expr
              if isMultiLineOrLongExpression expr
                then constructorString ++ " " ++ letBindingsString ++ "->\n" ++ increaseIndentation 1 expressionString
                else constructorString ++ " " ++ letBindingsString ++ "-> " ++ expressionString
              where
                showAlternativConstructor :: AltCon -> String
                showAlternativConstructor (LitAlt lit) = showLikeHaskell False (Lit lit)
                showAlternativConstructor (DataAlt dataCon) = showOutputable dataCon
                showAlternativConstructor DEFAULT = "_"

        showLam :: Bool -> ([Var], CoreExpr) -> String
        showLam isOutermostLevel (bindings, expr) = do
          let expressionString = showLikeHaskell True expr
          let cleanUpBindings = optionallyRemoveTypeVars removeTypes bindings
          let parameterStrings = map showVar cleanUpBindings
          let parameterListString = unwords parameterStrings
          let result =
                if isMultiLineOrLongExpression expr
                  then "\\" ++ parameterListString ++ " ->\n" ++ increaseIndentation 1 expressionString
                  else "\\" ++ parameterListString ++ " -> " ++ expressionString
          if isOutermostLevel
            then result
            else "(" ++ result ++ ")"

        showApplication :: Bool -> (Expr Var, [Expr Var]) -> String
        showApplication isOutermostLevel (function, arguments) = do
          let cleanUpArguments = optionallyRemoveTypeInformation removeTypes arguments
          let makeInlineFunctionApplication = isOperator function && (length cleanUpArguments == 2)
          let makeMultiLineApplication = any isMultiLineOrLongExpression (function : arguments)
          let functionExpression =
                if makeInlineFunctionApplication && not makeMultiLineApplication
                  then showOperatorWithoutBrackets function
                  else showLikeHaskell False function

          let result =
                if makeMultiLineApplication
                  then showMultiLineAppliation functionExpression (map (showLikeHaskell True) cleanUpArguments)
                  else showOneLineApplication makeInlineFunctionApplication functionExpression (map (showLikeHaskell False) cleanUpArguments)
          if isOutermostLevel
            then result
            else "(" ++ result ++ ")"
          where
            showMultiLineAppliation :: String -> [String] -> String
            showMultiLineAppliation functionExpression argumentsExpression = functionExpression ++ "\n" ++ increaseIndentation 1 (intercalate "\n" argumentsExpression)

            showOneLineApplication :: Bool -> String -> [String] -> String
            showOneLineApplication False functionExpression argumentsExpression = unwords (functionExpression : argumentsExpression)
            showOneLineApplication True functionExpression [firstArgument, secondArgument] = firstArgument ++ " " ++ functionExpression ++ " " ++ secondArgument
            showOneLineApplication True _ _ = error "inline function application needs exactly two arguments"

        showTuple :: [CoreExpr] -> String
        showTuple tupleArguments = do
          let cleanUpArguments = optionallyRemoveTypeInformation removeTypes tupleArguments
          let argumentsExpression = map (showLikeHaskell True) cleanUpArguments
          if any isMultiLineOrLongExpression tupleArguments
            then showMultiLineTuple argumentsExpression
            else showOneLineTuple argumentsExpression
          where
            showOneLineTuple :: [String] -> String
            showOneLineTuple arguments = "(" ++ intercalate ", " arguments ++ ")"

            showMultiLineTuple :: [String] -> String
            showMultiLineTuple arguments = do
              let separatedArguments = intercalate ",\n" arguments
              "(\n" ++ increaseIndentation 1 separatedArguments ++ ")"

        showCoreList :: [CoreExpr] -> String
        showCoreList listArguments = do
          let cleanUpArguments = optionallyRemoveTypeInformation removeTypes listArguments
          let argumentsExpression = map (showLikeHaskell True) cleanUpArguments
          if any isMultiLineOrLongExpression listArguments
            then showMultiLineList argumentsExpression
            else showOneLineList argumentsExpression
          where
            showOneLineList :: [String] -> String
            showOneLineList arguments = "[" ++ intercalate ", " arguments ++ "]"

            showMultiLineList :: [String] -> String
            showMultiLineList arguments = do
              let separatedArguments = intercalate ",\n" arguments
              "[\n" ++ increaseIndentation 1 separatedArguments ++ "\n]"

-- | takes a list of expressions and removes all type information if the first parameter is true
optionallyRemoveTypeInformation :: Bool -> [CoreExpr] -> [CoreExpr]
optionallyRemoveTypeInformation False expressions = removeTypeInformation expressions
optionallyRemoveTypeInformation True expressions = expressions

-- | takes a list of vars and removes all type information if the first parameter is true
optionallyRemoveTypeVars :: Bool -> [Var] -> [Var]
optionallyRemoveTypeVars False vars = removeTypeVars vars
optionallyRemoveTypeVars True vars = vars