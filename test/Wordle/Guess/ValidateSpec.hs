module Wordle.Guess.ValidateSpec (spec) where

import qualified Data.Set as S
import Test.Hspec
import Wordle.Game.Types (Game (..), feedbackToString)
import Wordle.Guess.Validate (validate, Validation (..))
import Wordle.Test.Data

spec :: Spec
spec = do
  describe "validate" $ do
    context "with a valid input" $
      it "returns no error" $
        validate cfg game "HELLO" `shouldBe` Nothing
    context "with an input longer than configured" $
      it "returns an Invalid Length error message" $
        validate cfg game "ABCDEF" `shouldBe` Just (InvalidLength "ABCDEF" 5)
    context "with an input shorter than configured" $
      it "returns an Invalid Length error message" $
        validate cfg game "ABCD" `shouldBe` Just (InvalidLength "ABCD" 5)
    context "with an input that has non-letters" $
      it "returns an Invalid Characters error message" $ do
        validate cfg game "ABCD1" `shouldBe` Just (InvalidCharacters $ S.singleton '1')
        validate cfg game "AB$DE" `shouldBe` Just (InvalidCharacters $ S.singleton '$')
    context "with an input that have been entered already" $
      it "returns an Already exist error message" $
        let s = feedbackToString fb
         in validate cfg game {guesses = [fb]} s `shouldBe` Just (AlreadyExists s)
    context "with an input that does not exist in the dictionary" $
      it "returns an Invalid Word error message" $
        validate cfg game "HELLL" `shouldBe` Just NotInDictionary


