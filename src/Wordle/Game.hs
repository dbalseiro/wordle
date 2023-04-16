module Wordle.Game (play) where

import Wordle.Game.Types (Config(..), Game(..), Outcome(..), GameSettings (..))
import Wordle.Game.Logic (updateGame, guessOutcome)

import Wordle.Config (getWord, getGameSettings)
import Wordle.Guess (askGuess)
import Wordle.Render (renderGuesses, gameWon, gameLost)


-- | Get configuration and play first turn
play :: IO ()
play = do
  setts <- getGameSettings
  config <- Config setts <$> getWord (dictionary setts)
  playTurn config initialGame

playTurn :: Config -> Game -> IO ()
playTurn config game = do
  game' <- updateGame config game <$> askGuess config game
  renderGuesses game'
  case guessOutcome config game' of
    Won -> gameWon
    OutOfTries -> gameLost config
    WrongGuess -> playTurn config game'

initialGame :: Game
initialGame = Game [] 0 []
