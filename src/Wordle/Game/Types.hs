module Wordle.Game.Types
  ( Game(..)
  , Config(..)
  , FeedbackUnit(..)
  , Feedback
  , Accuracy(..)
  , Guess
  , Outcome(..)
  ) where

data Game = Game
  { guesses :: ![Feedback]
  , try :: !Int
  , feedback :: !Feedback
  }

data Config = Config
  { tries :: !Int
  , word :: !String
  }

data FeedbackUnit = Feedback
  { letter :: !Char
  , accuracy :: !Accuracy
  }

type Feedback = [FeedbackUnit]
type Guess = String

data Accuracy = Correct | Incorrect | BadPosition

data Outcome = Won | WrongGuess | OutOfTries
