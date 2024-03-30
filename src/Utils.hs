module Utils where
import GHC.Plugins ( Outputable(ppr), showSDoc, text, OutputableBndr, Bind, CoreProgram )
import Data.Data ( Data )
import DynFlags ( baseDynFlags )
import GHC.Hs.Dump ( showAstData, BlankSrcSpan(BlankSrcSpan) )
import Data.Text (Text, pack)
import GHC.Core.Ppr (pprCoreBindings, pprCoreBinding)
import Control.Monad.Trans (liftIO, MonadIO)
import GHC (pprFunBind)

showOutputable :: Outputable a => a -> String
showOutputable = showSDoc baseDynFlags . ppr

textOutputable :: Outputable a => a -> Text
textOutputable = pack . showOutputable

printAst :: Data a => a -> String
printAst = showOutputable . showAstData  BlankSrcSpan

textAst :: Data a => a -> Text
textAst = pack . printAst

printCore :: (MonadIO m, OutputableBndr b) => [Bind b] -> m ()
printCore coreAst = liftIO (putStrLn (showOutputable (pprCoreBindings coreAst)))

listTopLevelFunctions :: CoreProgram -> IO ()
listTopLevelFunctions cp = do
    let topLevelNames = map (takeWhile (/= ' ') . showOutputable . pprCoreBinding) cp
    mapM_ putStrLn topLevelNames
