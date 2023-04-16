module Wordle.Game.LogicSpec (spec) where

import Test.Hspec
import Wordle.Test.Data (cfg, game)
import Wordle.Game.Logic
import Wordle.Game.Types

spec :: Spec
spec = do
  describe "updateGame" $ do
    it "adds a correct feedback" $
      let (game', ans) = losingGame
       in updateGame cfg game ans`shouldBe` game'
    it "adds a failing feedback" $
      let (game', ans) = winningGame
       in updateGame cfg game ans `shouldBe` game'
  describe "guessOutcome" $ do
    context "when you guess right" $ do
      it "returns Winning outcome" $
        let (game', _) = winningGame
        in guessOutcome cfg game' `shouldBe` Won
    context "when you guess wrong" $ do
      let (game', _) = losingGame
      context "and you still have tries left" $
        it "returns Wrong Guess outcome" $
          guessOutcome cfg { tries = 2 } game' `shouldBe` WrongGuess
      context "and you're out of tries" $
        it "returns Out Of Tries outcome" $
          guessOutcome cfg  game' `shouldBe` OutOfTries


winningGame :: (Game, String)
winningGame =
  let ans = "SAMPL"
      fb = map (`FeedbackUnit` Correct) ans
   in (Game [fb] 1 fb, ans)

losingGame :: (Game, String)
losingGame =
  let ans = "SELLO"
      fb = zipWith FeedbackUnit ans [Correct, Incorrect, BadPosition, BadPosition, Incorrect]
   in (Game [fb] 1 fb, ans)

