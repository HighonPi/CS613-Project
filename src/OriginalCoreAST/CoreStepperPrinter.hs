module OriginalCoreAST.CoreStepperPrinter (
    printCoreStepByStepReductionForBinding, printCoreStepByStepReductionForEveryBinding,
    convertToBindingsList, printCoreStepByStepReductionForSingleExpression,
  )
where

import OriginalCoreAST.CoreStepperHelpers.CoreLookup(tryFindBindingForString)
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator (convertFunctionApplicationWithArgumentListToNestedFunctionApplication, convertToMultiArgumentFunction)
import OriginalCoreAST.CoreInformationExtractorFunctions(varToString, canBeReduced, canBeReducedToNormalForm, isTypeInformation)
import OriginalCoreAST.CorePrettyPrinter(prettyPrint)
import OriginalCoreAST.CoreStepper(applyStep, safeReduceToNormalForm)
import OriginalCoreAST.CoreTypeDefinitions
  ( Binding,
    FunctionName,
    ReductionStepDescription (..),
    ReductionSuccessfulFlag (..),
    StepResult,
  )
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import GHC.Plugins
  ( Bind (NonRec, Rec),
    CoreBind,
    CoreExpr,
    Var (varName),
  )

-- printing

reductionsCompletedString = "All reductions completed"
noReductionsString = "No reduction rule implemented for this expression further"
maximumReductionsExceededString = "Maximum number of Reductions exceeded"

printCoreStepByStepReductionForEveryBinding :: Bool -> Maybe String -> [CoreBind] -> [CoreBind] -> IO()
printCoreStepByStepReductionForEveryBinding isVerbose functionToStep userBindings preludeBindings = do
    mapM_ (printCoreStepByStepReductionForBinding isVerbose allBindings) bindingsToStep
    if null bindingsToStep
        then do
            putStrLn "\nNo (Valid) binding is given to step into!"
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

printCoreStepByStepReductionForBinding :: Bool -> [Binding] -> Binding -> IO ()
printCoreStepByStepReductionForBinding isVerbose bindings (var, exp) = do
    putStr "\n**** Reduction of "
    putStr (varToString var)
    putStr " ****\n"
    prettyPrint exp
    printCoreStepByStepReductionForSingleExpression isVerbose bindings exp

-- | Maximum number of reductions before it's inferred that an infinite loop is present
maximumReductionLimit = 500

-- | Shows step-by-step reduction for a core expression 
printCoreStepByStepReductionForSingleExpression :: Bool -> [Binding] -> CoreExpr -> IO ()
printCoreStepByStepReductionForSingleExpression isVerbose bindings expression = do
  let (stepResults, reductionSuccessfulFlag) = getAllSteps maximumReductionLimit bindings expression
  printStepResultList isVerbose stepResults reductionSuccessfulFlag

-- | Prints the step-by-step reduction
printStepResultList :: Bool -> [StepResult] -> ReductionSuccessfulFlag -> IO ()
printStepResultList _ [] Success = putStrLn "\n---- All reductions completed\n"
printStepResultList _ [] NoReductionRule = putStrLn "\n---- Reduction chain completed: No reduction rule implemented for this expression\n"
printStepResultList _ _ StoppedToPreventInfiniteLoop = putStrLn "\n---- Exceeded maximum number of reductions. Infinite loop inference\n"
printStepResultList isVerbose ((stepDescription, expression, _) : xs) successFlat = do
    let ignorePrintingStep = (displayReductionStep isVerbose stepDescription)
    if (ignorePrintingStep || null xs)
        then do
            putStrLn ("\n---- " ++ show stepDescription)
            prettyPrint expression
        else return ()
    printStepResultList isVerbose xs successFlat

-- | Return a record of all sub-steps
getAllSteps :: Integer -> [Binding] -> CoreExpr -> ([StepResult], ReductionSuccessfulFlag)
getAllSteps 0 bindings expression = ([], StoppedToPreventInfiniteLoop)
getAllSteps maximumAmountOfSteps bindings expression = do
  if canBeReduced expression
    then do
      let maybeReduction = applyStep bindings expression
      case maybeReduction of
        Just (reductionStep, reducedExpression, newBindings) -> do
          let (stepDescription, reductionSuccessfulFlat) = getAllSteps (maximumAmountOfSteps - 1) newBindings reducedExpression
          ((reductionStep, reducedExpression, newBindings) : stepDescription, reductionSuccessfulFlat)
        Nothing -> ([], NoReductionRule)
    else do
      --try to reduce even more (only for visualization)
      if canBeReducedToNormalForm expression
        then do
          let (function, arguments) = convertToMultiArgumentFunction expression
          let maybeArgumentsInNormalForm = map (safeReduceToNormalForm bindings) arguments
          if any isNothing maybeArgumentsInNormalForm
            then ([], NoReductionRule)
            else do
              let argumentsInNormalForm = map fromJust maybeArgumentsInNormalForm
              let result = convertFunctionApplicationWithArgumentListToNestedFunctionApplication function argumentsInNormalForm
              ([(ConstructorArgumentReductionForVisualization, result, [])], Success)
        else ([], Success)

convertToBindingsList :: [CoreBind] -> [Binding]
convertToBindingsList bindings = concat (map convertCoreBindingToBindingList bindings)

convertCoreBindingToBindingList :: CoreBind -> [Binding]
convertCoreBindingToBindingList (NonRec binding exp) = [(binding, exp)]
convertCoreBindingToBindingList (Rec bindings) = bindings

displayReductionStep :: Bool -> ReductionStepDescription -> Bool
displayReductionStep _ (EvaluationStep _) = True
displayReductionStep _ PatternMatchStep = True
displayReductionStep _ ConstructorArgumentReductionForVisualization = True
displayReductionStep _ StrictApplicationArgumentStep = True
displayReductionStep isVerbose _ = isVerbose