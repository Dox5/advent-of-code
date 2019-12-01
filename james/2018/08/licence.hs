module Main(main) where

import qualified Data.List as List 

import qualified System.IO as IO

type ChildNodeCount = Int
type MetaDataCount  = Int
type MetaData       = Int

data Header = Header ChildNodeCount MetaDataCount deriving (Show)

data Node a = Node Header [Node a] [a] deriving (Show)



readsHeader :: String -> [(Header, String)]
readsHeader s = [(Header nChild nMeta, s3) | (nChild, s2) <- reads s,
                                             (nMeta, s3)  <- reads s2]

readN :: Read a => Int -> String -> [([a], String)]
readN 0 s = [([], s)]
readN count initial = [((map fst nParsed), snd . last $ nParsed)]
  where
    nParsed = x count initial []
    x :: Read a => Int -> String -> [(a, String)] -> [(a, String)]
    x 1 s    = ((head . reads $ s):)
    x left s =  ((a, s2):) . x (left-1) s2
      where
        parsed = head . reads $ s
        a      = fst parsed
        s2     = snd parsed

--recursively read a node structure
readsNode :: Read a => String -> [(Node a, String)]
readsNode s = [(Node hdr children meta, remaining)
                  | (hdr, s2) <- readsHeader s,
                    (children, s3) <- readChildren hdr s2,
                    (meta, remaining) <- readMeta hdr s3]
  where
    readChildren :: Read a => Header -> String -> [([Node a], String)]
    readChildren (Header count _) s = readN count s

    readMeta :: Read a => Header -> String -> [([a], String)]
    readMeta (Header _ count) s = readN count s


instance Read a => Read (Node a) where
  readsPrec _ s = readsNode s

foldrChildren :: (a -> b -> b) -> b -> [Node a] -> b
foldrChildren f z [] = z
foldrChildren f z (child:children) = foldr f (foldrChildren f z children) child


instance Foldable Node where
  foldr f z (Node hdr children meta) = foldr f (foldrChildren f z children) meta
      
sumMeta :: Node MetaData -> MetaData
sumMeta = foldr (+) 0

-- Test if v is in range [0, max)
inRange :: Ord a => (a, a) -> a -> Bool
inRange (min, max) v = min <= v && v < max

scoreNode :: Node MetaData -> Int
scoreNode (Node _ [] meta) = sum meta
scoreNode (Node _ children meta) = sum childScores
  where
    childAndCount is = (children !! (head is), length is)
    validIndex = inRange (0, (length children))
    scoringChildren = map childAndCount . List.group . List.sort . filter validIndex . map decrement $ meta
      where
        decrement x = x - 1
    childScores = map (\(child, n) -> n * (scoreNode child)) scoringChildren

getNodeTree :: IO.FilePath -> IO (Node MetaData)
getNodeTree path = do
  contents <- IO.readFile path
  return . read $ contents

main :: IO()
main = do
  nodes <- getNodeTree "input.txt"
  putStrLn . showString "Sum of metadata: " . shows (sumMeta nodes) $ ""
  putStrLn . showString "Root node score: " . shows (scoreNode nodes) $ ""
