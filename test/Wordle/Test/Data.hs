module Wordle.Test.Data (cfg, game, fb) where

import Wordle.Game.Types

cfg :: Config
cfg = Config gameSettings "SAMPL"

game :: Game
game = Game [] 0 []

fb :: Feedback
fb = map (`FeedbackUnit` Incorrect) "HELLO"

gameSettings :: GameSettings
gameSettings = GameSettings (WordLength 5) (Tries 1) ["SAMPL", "HELLO", "DIEGO"]
