module Wordle.Game (play) where

import Wordle.Game.Types (Config(..), Game(..), Outcome(..))
import Wordle.Game.Logic (updateGame, guessOutcome)

import Wordle.Config (getTries, getWord)
import Wordle.Guess (askGuess)
import Wordle.Render (renderGuesses, gameWon, gameLost)


play :: IO ()
play = do
  config <- Config <$> getTries <*> getWord
  playTurn config initialGame

playTurn :: Config -> Game -> IO ()
playTurn config game = do
  game' <- updateGame config game <$> askGuess
  renderGuesses config game'
  case guessOutcome config game' of
    Won -> gameWon
    OutOfTries -> gameLost
    WrongGuess -> playTurn config game'

initialGame :: Game
initialGame = Game [] 0 []
