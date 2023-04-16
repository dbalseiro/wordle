module Wordle.Utils (notImplemented, defaultRead) where
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

notImplemented :: a
{-# WARNING notImplemented "Not Implemented" #-}
notImplemented = error "Not Implemented"

defaultRead :: Read n => n -> Maybe String -> n
defaultRead n = fromMaybe n . (>>= readMaybe)
