module Wordle.Game.Types.Effects (WordleStateManagementM(..), WordleRandomM(..), WordleDisplayM(..)) where

import Wordle.Game.Types

class Monad m => WordleStateManagementM m where
  getSettings :: m GameSettings
  setGame :: Game -> m ()
  getGame :: m Game

class Monad m => WordleRandomM m where
  pickWord :: Dictionary -> m ()

class Monad m => WordleDisplayM m where
  displayPrompt :: String -> m ()
  getInput :: m String
  renderValidationError :: String -> m ()
  gameWon :: m ()
  gameLost :: m ()
  renderFeedback :: [FeedbackUnit] -> m ()

