{-# LANGUAGE NamedFieldPuns #-}

module Wordle.Game.Logic (updateGame, guessOutcome) where

import Wordle.Game.Types ( Config (..)
                        , Game (..)
                        , Guess
                        , Outcome(..)
                        , Feedback
                        , FeedbackUnit(..)
                        , Accuracy (..)
                        , feedbackToString
                        )

updateGame :: Config -> Game -> Guess -> Game
updateGame Config{word} Game{guesses, try} guess =
  let feedback = mkFeedback (zip word guess)
   in Game { guesses = feedback : guesses
           , try = try + 1
           , feedback
           }
  where
    mkFeedback :: [(Char, Char)] -> Feedback
    mkFeedback = map mkFeedbackUnit

    mkFeedbackUnit :: (Char, Char) -> FeedbackUnit
    mkFeedbackUnit (charFromGuess, charFromWord) =
      FeedbackUnit charFromGuess (evaluate charFromGuess charFromWord)

    evaluate :: Char -> Char -> Accuracy
    evaluate c1 c2
      | c1 == c2       = Correct
      | c1 `elem` word = BadPosition
      | otherwise      = Incorrect


guessOutcome :: Config -> Game -> Outcome
guessOutcome Config{word, tries} Game{feedback, try}
  | word == feedbackToString feedback = Won
  | try >= tries                      = OutOfTries
  | otherwise                         = WrongGuess
  
