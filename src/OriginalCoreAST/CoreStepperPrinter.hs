module OriginalCoreAST.CoreStepperPrinter
  (printCoreStepByStepReductionForBinding, printCoreStepByStepReductionForEveryBinding
  )
where

import OriginalCoreAST.CoreInformationExtractorFunctions(varToString, canBeReduced)
import OriginalCoreAST.CoreStepper(applyStep)
import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString)

import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import OriginalCoreAST.CorePrettyPrinter(prettyPrint)
import OriginalCoreAST.CoreStepperHelpers.CoreLookup(tryFindBindingForString)

type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (Var, Expr Var)

-- printing

reductionsCompletedString = "All reductions completed"
noReductionsString = "No reduction rule implemented for this expression further"
maximumReductionsExceededString = "Maximum number of Reductions exceeded"

printCoreStepByStepReductionForEveryBinding :: Maybe String -> [CoreBind] -> [CoreBind] -> IO()
printCoreStepByStepReductionForEveryBinding functionToStep userBindings preludeBindings = do
    mapM_ (printCoreStepByStepReductionForBinding allBindings) bindingsToStep
    if null bindingsToStep
        then do
            putStrLn "\nNo function is given to step into!"
        else
            return ()
    where
        userBindingsList = convertToBindingsList userBindings
        preludeBindingsList = convertToBindingsList preludeBindings
        allBindings = userBindingsList ++ preludeBindingsList
        
        bindingsToStep = do
            if isNothing functionToStep
            then
                userBindingsList
            else
                let response = tryFindBindingForString (fromJust functionToStep) allBindings
                in if isNothing response
                then
                    []
                else
                    [fromJust response]
                    
        -- bindingToStep = tryFindBindingForString (fromJust functionToStep) allBindings

maximumReductionLimit = 200

printCoreStepByStepReductionForBinding :: [Binding] -> Binding -> IO ()
printCoreStepByStepReductionForBinding bindings (var, exp) = do
    putStr "\n**** Reduction of "
    putStr (varToString var)
    putStr " ****\n"
    prettyPrint exp
    printAllStepsUptoALimit maximumReductionLimit (getCoreStepByStepReductionForSingleExpression bindings exp)
    -- printCoreStepByStepReductionForSingleExpression bindings exp

-- printCoreStepByStepReductionForSingleExpression :: [Binding] -> Expr Var -> IO()
-- printCoreStepByStepReductionForSingleExpression bindings expression     | canBeReduced expression = do
--                                                                             let reduction = (applyStep bindings expression)
--                                                                             case reduction of
--                                                                                 Just (reductionStepDescription, reducedExpression) -> do
--                                                                                     if ((/=) reductionStepDescription "unpackCString#") then do
--                                                                                         putStrLn ("\n-- " ++ reductionStepDescription)
--                                                                                         prettyPrint reducedExpression
--                                                                                     else
--                                                                                         return () -- do nothing, continue to the next statement
--                                                                                     printCoreStepByStepReductionForSingleExpression bindings reducedExpression
--                                                                                 Nothing -> putStrLn "\n-- No reduction rule implemented for this expression"
--                                                                         | otherwise = putStrLn "\n-- All reductions completed"

getCoreStepByStepReductionForSingleExpression :: [Binding] -> Expr Var -> [(ReductionStepDescription, Maybe (Expr Var))]
getCoreStepByStepReductionForSingleExpression bindings expression     | canBeReduced expression = do
                                                                            let reduction = (applyStep bindings expression)
                                                                            case reduction of
                                                                                Just (reductionStepDescription, reducedExpression) -> do
                                                                                    if ((/=) reductionStepDescription "unpackCString#")
                                                                                    then
                                                                                        (reductionStepDescription, Just reducedExpression) : (getCoreStepByStepReductionForSingleExpression bindings reducedExpression)
                                                                                    else
                                                                                        getCoreStepByStepReductionForSingleExpression bindings reducedExpression
                                                                                Nothing -> [(noReductionsString, Nothing)]
                                                                        | otherwise = []

printAllStepsUptoALimit:: Int -> [(ReductionStepDescription, Maybe (Expr Var))] -> IO ()
printAllStepsUptoALimit _ [] = do
    putStrLn ("\n---- " ++ reductionsCompletedString)

printAllStepsUptoALimit _ ((reductionDescp, Nothing): xs) = do
    putStrLn ("\n---- " ++ reductionDescp)

printAllStepsUptoALimit 0 _ = do
    putStrLn ("\n---- " ++ maximumReductionsExceededString)

printAllStepsUptoALimit limit ((reductionDescp, Just expressionToPrint): xs) = do
    putStrLn ("\n---- " ++ reductionDescp)
    prettyPrint expressionToPrint
    printAllStepsUptoALimit (limit - 1) xs

convertToBindingsList :: [CoreBind] -> [Binding]
convertToBindingsList bindings = concat (map convertCoreBindingToBindingList bindings)

convertCoreBindingToBindingList :: CoreBind -> [Binding]
convertCoreBindingToBindingList (NonRec binding exp) = [(binding, exp)]
convertCoreBindingToBindingList (Rec bindings) = bindings