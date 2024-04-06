module Main where
import Cli ( runCli, dispatch )

main :: IO ()
main = do
  invocation <- runCli
  print invocation
  dispatch invocation
