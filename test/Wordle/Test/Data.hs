module Wordle.Test.Data (cfg, game, fb) where

import Wordle.Game.Types

cfg :: Config
cfg = Config 1 "SAMPL" 5

game :: Game
game = Game [] 0 []

fb :: Feedback
fb = map (`FeedbackUnit` Incorrect) "HELLO"

