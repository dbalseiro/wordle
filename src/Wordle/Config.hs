module Wordle.Config (getTries, getWord, getLength) where

getTries :: IO Int
getTries = return 10

getWord :: IO String
getWord = return "DIEGO"

getLength :: IO Int
getLength = return 5

getDictionary :: IO [String]
getDictionary = undefined
