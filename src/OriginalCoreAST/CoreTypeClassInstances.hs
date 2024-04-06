module OriginalCoreAST.CoreTypeClassInstances()
where

import GHC.Core (Expr (..))
import GHC.Types.Literal(Literal (..))
import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression)

instance Num (Expr b) where
  (+) (Lit x) (Lit y) = Lit ((+) x y)
  (+) _ _ = error "+ not supported by this type"
  (-) (Lit x) (Lit y) = Lit ((-) x y)
  (-) _ _ = error "- not supported by this type"
  (*) (Lit x) (Lit y) = Lit ((*) x y)
  (*) _ _ = error "* not supported by this type"
  signum (Lit x) = Lit (signum x)
  signum _ = error "signum not supported by this type"
  fromInteger x = integerToCoreExpression x
  abs (Lit x) = Lit (abs x)
  abs _ = error "abs not supported by this type"

instance Fractional (Expr b) where
  (/) (Lit x) (Lit y) = Lit ((/) x y)
  (/) _ _ = error "/ not supported by this type"
  recip (Lit x) = Lit (recip x)
  recip _ = error "recip not supported by this type"
  fromRational x = rationalToCoreExpression x

instance Eq (Expr b) where
  (/=) (Lit x) (Lit y) = (/=) x y
  (/=) _ _ = error "/= not supported by this type"
  (==) (Lit x) (Lit y) = (==) x y
  (==) _ _ = error "== not supported by this type"

instance Ord (Expr b) where
  (<=) (Lit x) (Lit y) = (<=) x y
  (<=) _ _ = error "<= not supported by this type"
  (<) (Lit x) (Lit y) = (<=) x y
  (<) _ _ = error "< not supported by this type"
  (>=) (Lit x) (Lit y) = (<=) x y
  (>=) _ _ = error ">= not supported by this type"
  (>) (Lit x) (Lit y) = (<=) x y
  (>) _ _ = error "> not supported by this type"

instance Num Literal where
  (+) (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral ((Prelude.+) x y)
  (+) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.+) (fromInteger x) (fromRational y))
  (+) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromInteger y))
  (+) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.+) (fromInteger x) (fromRational y))
  (+) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromInteger y))
  (+) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) _ _ = error "+ not supported by this type"
  (-) (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral ((Prelude.-) x y)
  (-) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.-) (fromInteger x) (fromRational y))
  (-) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromInteger y))
  (-) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.-) (fromInteger x) (fromRational y))
  (-) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromInteger y))
  (-) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) _ _ = error "- not supported by this type"
  (*) (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral ((Prelude.*) x y)
  (*) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.*) (fromInteger x) (fromRational y))
  (*) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromInteger y))
  (*) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.*) (fromInteger x) (fromRational y))
  (*) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromInteger y))
  (*) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) _ _ = error "* not supported by this type"
  signum (LitNumber _ x) = integerToCoreLiteral (signum (fromInteger x))
  signum (LitDouble x) = fractionalToCoreLiteral (signum x)
  signum (LitFloat x) = fractionalToCoreLiteral (signum x)
  signum _ = error "signum not supported for this type"
  fromInteger x = integerToCoreLiteral x
  abs (LitNumber _ x) = integerToCoreLiteral (abs x)
  abs (LitDouble x) = fractionalToCoreLiteral (abs (fromRational x))
  abs (LitFloat x) = fractionalToCoreLiteral (abs (fromRational x))
  abs _ = error "abs not supported for this type"

instance Fractional Literal where
  (/) (LitNumber _ x) (LitNumber _ y) = LitDouble ((Prelude./) (fromInteger x) (fromInteger y))
  (/) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude./) (fromInteger x) y)
  (/) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude./) x (fromInteger y))
  (/) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude./) (fromInteger x) y)
  (/) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude./) x (fromInteger y))
  (/) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) _ _ = error "/ not supported by this type"
  recip expression = 1 / expression
  fromRational x = (LitDouble x)

instance Show Literal where
  show (LitChar charValue) = show charValue
  show (LitNumber _ integerValue) = show integerValue
  show (LitString stringValue) = show stringValue
  show (LitFloat floatValue) = show floatValue
  show (LitDouble doubleValue) = show doubleValue
  show (LitNullAddr) = "NULL"
  show (LitRubbish) = "(LitRubbish)"
  show (LitLabel fastString _ _) = "(LitLabel)"

