module OriginalCoreAST.CorePrettyPrinter(prettyPrint)
where

import GHC.Core (Expr (..))
import Utils (showOutputable)
import GHC.Types.Var (Var)

prettyPrint :: Expr Var -> IO()
prettyPrint exp = putStr (showOutputable exp)


