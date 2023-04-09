module Wordle.Guess (askGuess) where

import Wordle.Guess.Validate (validate)
import Wordle.Game.Types (Guess, Config, Game)
import Wordle.Render (renderValidationError)

askGuess :: Config -> Game -> IO Guess
askGuess config game = do
  putStr "Make a guess: "
  ans <- getLine
  case validate config game ans of
    Nothing -> return ans
    Just err -> do
      renderValidationError (show err)
      askGuess config game
