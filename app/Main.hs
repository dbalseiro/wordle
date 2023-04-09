module Main (main) where

import Wordle.Game (play)
import System.IO

main :: IO ()
main = do
   hSetBuffering stdin NoBuffering
   hSetBuffering stdout NoBuffering
   play
