{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Wordle.Render (renderGuesses, gameWon, gameLost, renderValidationError) where
import Wordle.Game.Types (Game (..), FeedbackUnit(..), Feedback, Accuracy (..), Config (..))

import System.Console.ANSI

--- | Render the feedback.
-- If a letter is in the correct position, render it with a green background
-- If the letter does not exist in the Configged word, then render it with a white background
-- If the letter exists anywhere else in the word, then render it with a yellow bg
renderGuesses :: Game -> IO ()
renderGuesses Game{guesses} = mapM_ renderFeedback $ reverse guesses

renderFeedback :: Feedback -> IO ()
renderFeedback feedback = do
  mapM_ renderFeedbackUnit feedback
  putStrLn ""

renderFeedbackUnit :: FeedbackUnit -> IO ()
renderFeedbackUnit FeedbackUnit{..} = do
  setSGR [SetColor Foreground Dull Black, SetConsoleIntensity BoldIntensity]
  case accuracy of
    Correct -> setSGR [SetColor Background Dull Green]
    Incorrect -> setSGR [SetColor Background Dull White]
    BadPosition -> setSGR [SetColor Background Dull Yellow]
  putStr (pure letter)
  setSGR [Reset]

gameWon :: IO ()
gameWon = putStrLn "YOU WON!! 🎉🎉🎉"

gameLost :: Config -> IO ()
gameLost Config{word} = do
  putStrLn "YOU LOST... the correct answer was:"
  setSGR [SetColor Foreground Dull White, SetConsoleIntensity BoldIntensity]
  putStrLn word
  setSGR [Reset]

renderValidationError :: String -> IO ()
renderValidationError err = do
  setSGR [SetColor Foreground Dull Red, SetConsoleIntensity BoldIntensity]
  putStrLn err
  setSGR [Reset]

