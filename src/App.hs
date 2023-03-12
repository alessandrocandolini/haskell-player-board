module App where

import Args ( Args, parseArgs )
import Options.Applicative ( handleParseResult )
import System.Environment (getArgs)
import Brick

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= uiProgram

program' :: Args -> IO()
program' = const (putStrLn "hello!")

uiProgram :: Args -> IO ()
uiProgram _ = simpleMain ui

ui :: Widget ()
ui = str "Hello, world!"



