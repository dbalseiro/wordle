module Wordle.Render (renderGuesses) where
import Wordle.Game.Types (Game (..))
import Wordle.Game.Types.Effects (WordleM(..))

--- | Render the feedback.
-- If a letter is in the correct position, render it with a green background
-- If the letter does not exist in the Configged word, then render it with a white background
-- If the letter exists anywhere else in the word, then render it with a yellow bg
renderGuesses :: WordleM m => m ()
renderGuesses = do
  g <- guesses <$> getGame
  mapM_ renderFeedback $ reverse g

