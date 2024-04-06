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

type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (Var, Expr Var)

-- printing

printCoreStepByStepReductionForEveryBinding :: [CoreBind] -> IO()
printCoreStepByStepReductionForEveryBinding bindings = do
    let allBindings = convertToBindingsList bindings
    mapM_ (printCoreStepByStepReductionForBinding allBindings) allBindings

printCoreStepByStepReductionForBinding :: [Binding] -> Binding -> IO ()
printCoreStepByStepReductionForBinding bindings (var, exp) = do
    putStr "\n**** Reduction of "
    putStr (varToString var)
    putStr " ****\n"
    prettyPrint exp
    printCoreStepByStepReductionForSingleExpression bindings exp

printCoreStepByStepReductionForSingleExpression :: [Binding] -> Expr Var -> IO()
printCoreStepByStepReductionForSingleExpression bindings expression     | canBeReduced expression = do
                                                                            let reduction = (applyStep bindings expression)
                                                                            case reduction of
                                                                                Just (reductionStepDescription, reducedExpression) -> do
                                                                                    if ((/=) reductionStepDescription "unpackCString#") then do
                                                                                        putStrLn ("\n-- " ++ reductionStepDescription)
                                                                                        prettyPrint reducedExpression
                                                                                    else
                                                                                        return () -- do nothing, continue to the next statement
                                                                                    printCoreStepByStepReductionForSingleExpression bindings reducedExpression
                                                                                Nothing -> putStrLn "\n-- No reduction rule implemented for this expression"
                                                                        | otherwise = putStrLn "\n-- All reductions completed"


convertToBindingsList :: [CoreBind] -> [Binding]
convertToBindingsList bindings = concat (map convertCoreBindingToBindingList bindings)

convertCoreBindingToBindingList :: CoreBind -> [Binding]
convertCoreBindingToBindingList (NonRec binding exp) = [(binding, exp)]
convertCoreBindingToBindingList (Rec bindings) = bindings