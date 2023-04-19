module Wordle.Test.Data (gameSettings, game, fb) where

import Wordle.Game.Types

game :: Game
game = Game [] 0 [] ""

fb :: Feedback
fb = map (`FeedbackUnit` Incorrect) "HELLO"

gameSettings :: GameSettings
gameSettings = GameSettings (WordLength 5) (Tries 1) ["SAMPL", "HELLO", "DIEGO"]
