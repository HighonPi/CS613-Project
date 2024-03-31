module OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression)
where

import GHC.Core (Expr (..))
import GHC.Types.Literal(Literal (..), mkLitInt64, mkLitString)
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import GHC.Types.Name(nameUnique, Name, mkSystemVarName, mkSysTvName, mkSystemName, pprNameUnqualified, nameStableString, getOccString)
import GHC.Types.Unique (minLocalUnique)
import GHC.Data.FastString (mkFastString)
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Types.Id.Info ( vanillaIdInfo, IdDetails(..))
import GHC.Types.Name.Occurrence (mkOccName, mkVarOcc)

integerToCoreLiteral :: Integer -> Literal
integerToCoreLiteral = mkLitInt64

fractionalToCoreLiteral :: Real a => a -> Literal
fractionalToCoreLiteral value = (LitDouble (toRational value))

rationalToCoreExpression :: Rational -> Expr b
rationalToCoreExpression value = Lit (LitDouble value)

integerToCoreExpression :: Integer -> Expr b
integerToCoreExpression value = Lit (mkLitInt64 value)

stringToCoreExpression :: String -> Expr Var
stringToCoreExpression value = Lit (mkLitString value)

boolToCoreExpression :: Bool -> Expr Var  --this is a hack, we just took the easiest constructors we found to create a "Var"-Instance without understanding what those constructors stand for 
boolToCoreExpression True = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc ("True"))) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)
boolToCoreExpression False = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc ("False"))) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)
