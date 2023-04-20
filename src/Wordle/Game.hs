module Wordle.Game (play) where

import Wordle.Game.Types (Outcome (..))
import Wordle.Config (getGameSettings, getWord)
import Wordle.Game.Types.Impl (runWordleT)
import Wordle.Guess (askGuess)
import Wordle.Game.Logic (updateGame, guessOutcome)
import Wordle.Render (renderGuesses)
import Wordle.Game.Types.Effects (WordleStateManagementM (..), WordleDisplayM (..))

-- | Get configuration and play first turn
play :: IO ()
play = do
  setts <- getGameSettings
  runWordleT setts $ do
    getWord
    playTurn

playTurn :: (WordleStateManagementM m, WordleDisplayM m) => m ()
playTurn = do
  settings <- getSettings
  guess <- askGuess
  game <- updateGame guess <$> getGame
  setGame game
  renderGuesses
  case guessOutcome settings game of
    Won -> gameWon
    OutOfTries -> gameLost
    WrongGuess -> playTurn
