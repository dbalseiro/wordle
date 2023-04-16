import qualified Data.List as L

main :: IO ()
main = do
  content <- map (surround ('"', '"')) . L.lines <$> readFile "db"
  let l = L.intercalate ", " content
  writeFile "db.json" (surround ('[', ']') l)

surround :: (a, a) -> [a] -> [a]
surround (l, r) a = (l:a) ++ [r]
