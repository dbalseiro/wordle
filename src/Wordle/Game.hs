module Wordle.Game (play) where

import Wordle.Game.Types (Outcome (..))
import Wordle.Config (getGameSettings, getWord)
import Wordle.Game.Types.Effects (WordleM (..))
import Wordle.Game.Types.Impl (runWordleT)
import Wordle.Guess (askGuess)
import Wordle.Game.Logic (updateGame, guessOutcome)
import Wordle.Render (renderGuesses)

-- | Get configuration and play first turn
play :: IO ()
play = do
  setts <- getGameSettings
  runWordleT setts $ do
    getWord
    playTurn

playTurn :: WordleM m => m ()
playTurn = do
  settings <- getSettings
  guess <- askGuess
  game <- getGame >>= setGame . updateGame guess
  renderGuesses
  case guessOutcome settings game of
    Won -> gameWon
    OutOfTries -> gameLost
    WrongGuess -> playTurn
