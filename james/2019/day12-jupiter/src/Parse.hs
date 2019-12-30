module Parse where


import Text.ParserCombinators.ReadP (ReadP, (+++), optional, char, skipSpaces, many1, satisfy)

type Vector = (Int, Int, Int)

readNum :: ReadP Int
readNum = num +++ negNum
  where
    num :: ReadP Int
    num = do
      s <- many1 $ satisfy (\c -> any (c==) "0123456789")
      return $ read s

    negNum :: ReadP Int
    negNum = do
      _ <- char '-'
      n <- num
      return $ -n

readComponent :: Char -> ReadP Int
readComponent c = do
  skipSpaces
  _ <- char c
  skipSpaces
  _ <- char '='
  skipSpaces
  n <- readNum
  optional $ char ','
  return n

readVector :: ReadP Vector
readVector = do
  _ <- char '<'
  x:y:z:[] <- mapM readComponent ['x', 'y', 'z']
  _ <- char '>'

  return (x, y, z)
