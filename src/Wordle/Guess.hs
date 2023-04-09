module Wordle.Guess (askGuess) where

import Wordle.Guess.Validate (validate)
import Wordle.Game.Types (Guess, Config, Game)
import Wordle.Render (renderValidationError)
import Data.Char (toUpper)

askGuess :: Config -> Game -> IO Guess
askGuess config game = do
  putStr "Make a guess: "
  ans <- fmap toUpper <$> getLine
  case validate config game ans of
    Nothing -> return ans
    Just err -> do
      renderValidationError (show err)
      askGuess config game
