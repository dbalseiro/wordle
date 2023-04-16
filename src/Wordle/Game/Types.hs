module Wordle.Game.Types
  ( Game(..)
  , Config(..)
  , FeedbackUnit(..)
  , Feedback
  , Accuracy(..)
  , Guess
  , Outcome(..)
  , feedbackToString
  ) where

data Game = Game
  { guesses  :: ![Feedback]
  , try      :: !Int
  , feedback :: !Feedback
  } deriving (Eq, Show)

data Config = Config
  { tries      :: !Int
  , word       :: !String
  , wordLength :: !Int
  }

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
