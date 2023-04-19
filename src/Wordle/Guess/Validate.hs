{-# LANGUAGE NamedFieldPuns #-}
module Wordle.Guess.Validate (validate, Validation(..)) where

import Data.Char (isLetter)
import Data.Set (Set)
import qualified Data.Set as S

import Wordle.Game.Types (Game (..), Guess, feedbackToString, WordLength (WordLength), GameSettings (..))
import Control.Applicative ((<|>))

data Validation = InvalidLength !String !Int | InvalidCharacters (Set Char) | AlreadyExists String | NotInDictionary
  deriving Eq

instance Show Validation where
  show (InvalidLength s l) = "Expecting answer of length " ++ show l ++ ", got " ++ s ++ " of length " ++ show (length s)
  show (InvalidCharacters s) = "There are invalid characters in your answer: [ " ++ S.toList s ++ " ]"
  show (AlreadyExists s) = "You already tried with " ++ s
  show NotInDictionary = "Not in the current dictionary. Try again"

validate :: GameSettings -> Game -> Guess -> Maybe Validation
validate settings Game{guesses} guess = tryAlreadyExist <|> tryInvalidCharacters <|> tryInvalidLength <|> tryNotInDictionary
  where
    tryInvalidCharacters =
      let s = S.fromList (filter (not . isLetter) guess)
       in if S.null s
            then Nothing
            else Just (InvalidCharacters s)

    tryInvalidLength | WordLength l <- wordLength settings, length guess /= l = Just (InvalidLength guess l)
                     | otherwise = Nothing

    tryAlreadyExist =
      let previous = map feedbackToString guesses
       in if guess `elem` previous
            then Just (AlreadyExists guess)
            else Nothing

    tryNotInDictionary | guess `notElem` dictionary settings = Just NotInDictionary
                       | otherwise = Nothing
