{-# LANGUAGE NamedFieldPuns #-}

module Wordle.Game.Logic (updateGame, guessOutcome) where

import Wordle.Game.Types ( Game (..)
                         , Guess
                         , Outcome(..)
                         , Feedback
                         , FeedbackUnit(..)
                         , Accuracy (..)
                         , feedbackToString, Tries (..), GameSettings (..)
                         )

updateGame :: Game -> Guess -> Game
updateGame Game{word, guesses, try} guess =
  let feedback = mkFeedback (zip word guess)
   in Game { guesses = feedback : guesses
           , try = try + 1
           , feedback
           , word
           }
  where
    mkFeedback :: [(Char, Char)] -> Feedback
    mkFeedback = map mkFeedbackUnit

    mkFeedbackUnit :: (Char, Char) -> FeedbackUnit
    mkFeedbackUnit (charFromWord, charFromGuess) =
      FeedbackUnit charFromGuess (evaluate charFromGuess charFromWord)

    evaluate :: Char -> Char -> Accuracy
    evaluate c1 c2
      | c1 == c2       = Correct
      | c1 `elem` word = BadPosition
      | otherwise      = Incorrect

guessOutcome :: GameSettings -> Game -> Outcome
guessOutcome settings Game{feedback, try, word}
  | word == feedbackToString feedback = Won
  | try >= (unTries . tries) settings = OutOfTries
  | otherwise = WrongGuess
