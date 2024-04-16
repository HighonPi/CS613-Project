module Main where

import Cli ( runCli, dispatch )
import Control.Exception (catch, SomeException, AsyncException(..), fromException)
import Control.Monad (forever)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "Press Ctrl-C to exit."
  loop

loop :: IO ()
loop = forever $ do
  putStr "Enter command: "
  hFlush stdout  -- Ensures that the prompt is displayed immediately
  command <- getLine
    -- Mimic the command line argument parsing
  catch (runCli command >>= dispatch) handleExceptions

handleExceptions :: SomeException -> IO ()
handleExceptions e = case fromException e of
  Just UserInterrupt-> putStrLn "Haskell Reduction Visalizer tool Exiting..."
  _                 -> putStrLn $ "Error occurred: " ++ show e