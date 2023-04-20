{-# LANGUAGE LambdaCase #-}
module Wordle.Guess (askGuess) where

import Wordle.Guess.Validate (validate)
import Wordle.Game.Types (Guess)
import Data.Char (toUpper)
import Wordle.Game.Types.Effects (WordleStateManagementM(..), WordleDisplayM (..))

askGuess :: (WordleDisplayM m, WordleStateManagementM m) => m Guess
askGuess = do
  displayPrompt "Make a guess: "
  ans <- fmap toUpper <$> getInput
  (validate <$> getSettings <*> getGame <*> pure ans) >>= \case
    Nothing -> return ans
    Just err -> do
      renderValidationError (show err)
      askGuess
