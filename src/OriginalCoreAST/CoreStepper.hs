module OriginalCoreAST.CoreStepper(applyStep)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.List(isPrefixOf, find)
import SimplifiedCoreAST.SimplifiedCoreASTPrinter (printSimplifiedCoreExpression)
import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr, showSDoc, showSDocUnsafe)
import GHC.Core.Utils (exprIsHNF)
import Utils (showOutputable)
import GHC.Core.Ppr
  ( pprCoreAlt,
  )
import Data.List(isPrefixOf)
import GHC.Types.Name(nameUnique, Name, mkSystemVarName, mkSysTvName, mkSystemName, pprNameUnqualified, nameStableString, getOccString)
import GHC.Types.Unique (minLocalUnique)
import GHC.Data.FastString (mkFastString)
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Types.Id.Info ( vanillaIdInfo, IdDetails(..))
import GHC.Types.Name.Occurrence (mkOccName, mkVarOcc)
import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced)
import OriginalCoreAST.CoreStepperHelpers.CoreEvaluator(evaluateFunctionWithArguments)
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator(convertFunctionApplicationWithArgumentListToNestedFunctionApplication, deepReplaceVarWithinExpression, deepReplaceVarWithinAlternative, deepReplaceMultipleVarWithinExpression, convertToMultiArgumentFunction)
import OriginalCoreAST.CoreStepperHelpers.CoreLookup(tryFindBinding, findMatchingPattern)

type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (Var, Expr Var)

applyStep :: [Binding] -> Expr Var -> Maybe (ReductionStepDescription, Expr Var)
applyStep bindings (Var name) = do
    foundBinding <- tryFindBinding name bindings
    return (("Replace '" ++ (varToString name) ++ "' with definition"),foundBinding) {-replace binding reference with actual expression (Delta Reduction)-}                                
applyStep bindings (App (Lam parameter expression) argument) = do
    Just ("Lamda Application", deepReplaceVarWithinExpression parameter argument expression)
applyStep bindings (App (App first second) third) = do
    applyStepToNestedApp bindings (App (App first second) third) --nested app
applyStep bindings (App (Var name) argument) = do
    let expression = tryFindBinding name bindings
    if isNothing expression 
        then do
            applyStepToNestedApp bindings (App (Var name) argument)
        else Just ("Replace '" ++ (varToString name) ++ "' with definition", (App (fromJust expression) argument))
applyStep bindings (Case expression binding caseType alternatives) = do
    if (canBeReduced expression) 
        then do 
            (description, reducedExpression) <- applyStep bindings expression
            return (description, (Case reducedExpression binding caseType alternatives))
        else do
            matchingPattern <- findMatchingPattern expression alternatives
            return ("Replace with matching pattern", matchingPattern)
applyStep _ _ = do
    Nothing


applyStepToNestedApp :: [Binding] -> Expr Var -> Maybe (ReductionStepDescription, Expr Var) 
applyStepToNestedApp bindings expr = do
    let (function, arguments) = (convertToMultiArgumentFunction expr)
    case function of
        (Var var) -> if (isNothing (tryFindBinding var bindings))
                        then if (any canBeReduced arguments) 
                            then do 
                                (description, simplifiedArguments) <- (applyStepToOneOfTheArguments bindings [] arguments)
                                return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication function simplifiedArguments)
                            else do
                                appliedFunction <- evaluateFunctionWithArguments function arguments --all arguments are reduced, eval function. This is stric behaviour! We have to use strict behaviour here because we are trying to evaluate a function whose definition we do not know. therefor we cannot apply the arguments one after another but have to simplify all arguments before calling the function 
                                return ("Apply " ++ (showOutputable function), appliedFunction) 
                        else do
                            (description, reducedFunction) <- applyStep bindings (App function (head arguments))
                            return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments))                            
        (Lam _ _) -> do
            (description, reducedFunction) <- applyStep bindings (App function (head arguments))
            return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments))

applyStepToOneOfTheArguments :: [Binding] -> [Expr Var] -> [Expr Var] -> Maybe (ReductionStepDescription, [Expr Var])
applyStepToOneOfTheArguments bindings alreadyReducedArguments (x:xs) = if canBeReduced x 
                                                                        then do
                                                                            (description, reducedArgument) <- applyStep bindings x
                                                                            return (description, (alreadyReducedArguments ++ [reducedArgument]) ++ xs)
                                                                        else applyStepToOneOfTheArguments bindings (alreadyReducedArguments ++ [x]) xs
applyStepToOneOfTheArguments bindings alreadyReducedArguments [] = error "no reducable argument found" --no argument that can be reduced was found. this should not happen because this condition gets checked earlier in the code


