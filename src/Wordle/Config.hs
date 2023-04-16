module Wordle.Config (getDictionary, getGameSettings, getWord) where

import Wordle.Utils (notImplemented, defaultRead)
import Wordle.Game.Types ( GameSettings (..)
                         , WordLength (..)
                         , Tries (..)
                         , Dictionary
                         , WordleException (..)
                         )

import System.Environment (lookupEnv)
import System.Random (randomRIO)

import Control.Exception (throwIO)

getGameSettings :: IO GameSettings
getGameSettings = GameSettings <$> getWordLength <*> getTries
  where
    getTries :: IO Tries
    getTries = Tries . defaultRead 5 <$> lookupEnv "WORDLE_TRIES"

    getWordLength :: IO WordLength
    getWordLength = WordLength . defaultRead 5 <$> lookupEnv "WORDLE_LENGTH"

getWord :: Dictionary -> IO String
getWord dict =
  let limit = length dict
   in case limit of
        0 -> throwIO EmptyDictionary
        _ -> (dict !!) <$> randomRIO (0, limit)

getDictionary :: WordLength -> IO Dictionary
getDictionary = notImplemented
