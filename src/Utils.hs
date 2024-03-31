module Utils where
import GHC.Plugins ( Outputable(ppr), showSDoc )
import Data.Data ( Data )
import DynFlags ( baseDynFlags )
import GHC.Hs.Dump ( showAstData, BlankSrcSpan(BlankSrcSpan) )

showOutputable :: Outputable a => a -> String
showOutputable = showSDoc baseDynFlags . ppr

printAst :: Data a => a -> String
printAst = showOutputable . showAstData  BlankSrcSpan
