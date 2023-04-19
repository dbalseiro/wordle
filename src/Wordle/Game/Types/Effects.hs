module Wordle.Game.Types.Effects (WordleM(..)) where

import Wordle.Game.Types

class Monad m => WordleM m where
  getSettings :: m GameSettings
  setGame :: Game -> m Game
  getGame :: m Game
  throwError :: WordleException -> m a
  pickWord :: Dictionary -> m ()
  displayPrompt :: String -> m ()
  getInput :: m String
  renderValidationError :: String -> m ()
  gameWon :: m ()
  gameLost :: m ()
  renderFeedback :: [FeedbackUnit] -> m ()

