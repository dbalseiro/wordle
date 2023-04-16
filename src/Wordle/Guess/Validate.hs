{-# LANGUAGE NamedFieldPuns #-}
module Wordle.Guess.Validate (validate, Validation(..)) where

import Data.Char (isLetter)
import Data.Set (Set)
import qualified Data.Set as S

import Wordle.Game.Types (Config (..), Game (..), Guess, feedbackToString)
import Control.Applicative ((<|>))

data Validation = InvalidLength !String !Int | InvalidCharacters (Set Char) | AlreadyExists String
  deriving Eq

instance Show Validation where
  show (InvalidLength s l) = "Expecting answer of length " ++ show l ++ ", got " ++ s ++ " of length " ++ show (length s)
  show (InvalidCharacters s) = "There are invalid characters in your answer: [ " ++ S.toList s ++ " ]"
  show (AlreadyExists s) = "You already tried with " ++ s

validate :: Config -> Game -> Guess -> Maybe Validation
validate Config{wordLength} Game{guesses} guess = tryAlreadyExist <|> tryInvalidCharacters <|> tryInvalidLength
  where
    tryInvalidCharacters =
      let s = S.fromList (filter (not . isLetter) guess)
       in if S.null s
            then Nothing
            else Just (InvalidCharacters s)

    tryInvalidLength | length guess /= wordLength = Just (InvalidLength guess wordLength)
                     | otherwise = Nothing

    tryAlreadyExist =
      let previous = map feedbackToString guesses
       in if guess `elem` previous
            then Just (AlreadyExists guess)
            else Nothing

