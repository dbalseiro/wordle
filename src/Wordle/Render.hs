module Wordle.Render (renderGuesses, gameWon, gameLost) where
import Wordle.Game.Types (Config, Game)
import Wordle.Utils (notImplemented)

renderGuesses :: Config -> Game -> IO ()
renderGuesses = notImplemented

gameWon :: IO ()
gameWon = notImplemented

gameLost :: IO ()
gameLost = notImplemented
