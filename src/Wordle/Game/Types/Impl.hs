{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Wordle.Game.Types.Impl (WordleT(..), runWordleT) where

import Wordle.Game.Types.Effects (WordleM(..))
import Wordle.Game.Types (Game(..), GameSettings, initialGame, FeedbackUnit (..), Accuracy (..))

import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.State (StateT, evalStateT, modify, MonadState, put, get)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (throwIO)

import System.Random (randomRIO)
import System.Console.ANSI

newtype WordleT a = WordleT { unWordleT :: ReaderT GameSettings (StateT Game IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Game, MonadReader GameSettings)

runWordleT :: GameSettings -> WordleT a -> IO a
runWordleT settings = flip evalStateT initialGame . flip runReaderT settings . unWordleT

instance WordleM WordleT where
  setGame g = put g >> return g
  getGame = get

  getSettings = ask

  throwError = liftIO . throwIO

  pickWord dict = do
    i <- liftIO $ randomRIO (0, length dict)
    modify (\game -> game { word = dict !! i })

  displayPrompt = liftIO . putStr
  getInput = liftIO getLine

  renderValidationError err = liftIO $ do
    setSGR [SetColor Foreground Dull Red, SetConsoleIntensity BoldIntensity]
    putStrLn err
    setSGR [Reset]

  gameWon = liftIO $ putStrLn "YOU WON!! 🎉🎉🎉"

  renderFeedback fb = do
    liftIO $ do
      mapM_ renderFeedbackUnit fb
      putStrLn ""
    where
      renderFeedbackUnit FeedbackUnit{..} = do
        setSGR [SetColor Foreground Dull Black, SetConsoleIntensity BoldIntensity]
        case accuracy of
          Correct -> setSGR [SetColor Background Dull Green]
          Incorrect -> setSGR [SetColor Background Dull White]
          BadPosition -> setSGR [SetColor Background Dull Yellow]
        putStr (pure letter)
        setSGR [Reset]

  gameLost = do
    w <- word <$> getGame
    liftIO $ do
      putStrLn "YOU LOST... the correct answer was:"
      setSGR [SetColor Foreground Dull White, SetConsoleIntensity BoldIntensity]
      putStrLn w
      setSGR [Reset]

