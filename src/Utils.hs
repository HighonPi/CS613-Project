module Utils where

import Control.Monad.IO.Class (MonadIO)
import Data.Data (Data)
import Data.Text (Text, pack)
import DynFlags (baseDynFlags)
import GHC.Core.Ppr (pprCoreBinding, pprCoreBindings)
import GHC.Hs.Dump (BlankSrcSpan (BlankSrcSpan), showAstData)
import GHC.Plugins
  ( Bind,
    CoreProgram,
    Outputable (ppr),
    OutputableBndr,
    liftIO,
    showSDoc,
  )

showOutputable :: Outputable a => a -> String
showOutputable = showSDoc baseDynFlags . ppr

textOutputable :: Outputable a => a -> Text
textOutputable = pack . showOutputable

printAst :: Data a => a -> String
printAst = showOutputable . showAstData BlankSrcSpan

textAst :: Data a => a -> Text
textAst = pack . printAst

printCore :: (MonadIO m, OutputableBndr b) => [Bind b] -> m ()
printCore coreAst = liftIO (putStrLn (showOutputable (pprCoreBindings coreAst)))

listTopLevelFunctions :: CoreProgram -> IO ()
listTopLevelFunctions cp = do
  let topLevelNames = map (takeWhile (/= ' ') . showOutputable . pprCoreBinding) cp
  mapM_ putStrLn topLevelNames

splitList :: [a] -> ([a], [a])
splitList myList = splitAt (((length myList) + 1) `div` 2) myList

-- | replaces a sequence of elements in a list with another sequence
-- For string replacements
-- Reference: https://bluebones.net/2007/01/replace-in-haskell/
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
  if take (length find) s == find
    then repl ++ (replace (drop (length find) s) find repl)
    else [head s] ++ (replace (tail s) find repl)


-- | takes a multi-line string and adds spaces before each line
increaseIndentation :: Integer -> String -> String
increaseIndentation 0 text = text
increaseIndentation x text = increaseIndentation (x - 1) indentedText
  where
    indentedText = "  " ++ replace text "\n" "\n  "