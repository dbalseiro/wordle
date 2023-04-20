module Wordle.Config (getGameSettings, getWord) where

import Wordle.Utils (defaultRead)
import Wordle.Game.Types ( GameSettings (..)
                         , WordLength (..)
                         , Tries (..)
                         , Dictionary
                         , WordleException (..)
                         )

import System.Environment (lookupEnv)

import Control.Exception (throwIO)
import Wordle.Game.Types.Effects

getGameSettings :: IO GameSettings
getGameSettings = do
  wl <- getWordLength
  GameSettings wl <$> getTries <*> getDictionary wl
  where
    getTries :: IO Tries
    getTries = Tries . defaultRead 5 <$> lookupEnv "WORDLE_TRIES"

    getWordLength :: IO WordLength
    getWordLength = WordLength . defaultRead 5 <$> lookupEnv "WORDLE_LENGTH"

getDictionary :: WordLength -> IO Dictionary
getDictionary (WordLength 5) = lines <$> readFile "db"
getDictionary (WordLength n) = throwIO (WordLengthNotImplemented n)

getWord :: (WordleStateManagementM m, WordleRandomM m) => m ()
getWord = do
  dict <- dictionary <$> getSettings
  pickWord dict
