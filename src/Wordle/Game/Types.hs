module Wordle.Game.Types
  ( Game(..)
  , FeedbackUnit(..)
  , Feedback
  , Accuracy(..)
  , Guess
  , Outcome(..)
  , GameSettings (..)
  , Tries (..)
  , WordLength (..)
  , Dictionary
  , WordleException (..)
  , feedbackToString
  , initialGame
  ) where

import Control.Exception (Exception)

data Game = Game
  { guesses  :: ![Feedback]
  , try      :: !Int
  , feedback :: !Feedback
  , word     :: !String
  } deriving (Eq, Show)

initialGame :: Game
initialGame = Game [] 0 [] ""

data FeedbackUnit = FeedbackUnit
  { letter   :: !Char
  , accuracy :: !Accuracy
  } deriving (Eq, Show)

type Feedback = [FeedbackUnit]
type Guess = String

data Accuracy = Correct | Incorrect | BadPosition
  deriving (Eq, Show)

data Outcome = Won | WrongGuess | OutOfTries
  deriving (Eq, Show)

feedbackToString :: Feedback -> String
feedbackToString = map letter

data GameSettings = GameSettings
  { wordLength :: WordLength
  , tries      :: Tries
  , dictionary :: Dictionary
  }

newtype WordLength = WordLength { unWordLength :: Int }

newtype Tries = Tries { unTries :: Int }

type Dictionary = [String]

data WordleException = EmptyDictionary | WordLengthNotImplemented Int

instance Exception WordleException

instance Show WordleException where
  show EmptyDictionary = "Dictionary is empty"
  show (WordLengthNotImplemented n) = "Words of length " ++ show n ++ " are not implemented yet"


