module Lib where

import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified Data.List.Key as ListKey
import qualified Data.List as List

type Width = Int
type Height = Int
type Depth = Int

newtype Pixel = Pixel Int
  deriving (Show, Eq, Ord)

toInteger :: Pixel -> Int
toInteger (Pixel v) = v

-- Pixels are stored left-to-right
data Row = Row {
  pixels   :: [Pixel],
  rowWidth :: Width
}
  deriving (Show)

-- Rows are stored top-to-bottom
data Layer = Layer {
  rows        :: [Row],
  layerHeight :: Height
}
  deriving (Show)

-- Images are stored with the left-most layer being the front-most layer
data Image = Image {
  layers :: [Layer],
  imageDepth :: Depth
}
  deriving (Show)

data RasterImage = RasterImage {
  rasterImage :: Layer
}
  deriving (Show)

type Stack = [Pixel]

pop :: State Stack Pixel
pop = do
  s <- State.get
  case s of
    (x:xs) -> do
      State.put xs
      return x
    _ -> error "Stack issues son"

popN :: Int -> State Stack [Pixel]
popN n = State.replicateM n pop

fromList :: [Pixel] -> Stack
fromList = id

nullStack :: State Stack Bool
nullStack = do
  s <- State.get
  return $ null s

consumeRow :: Width -> State Stack Row
consumeRow w = do
  ps <- popN w
  return $ Row ps w

consumeLayer :: Width -> Height -> State Stack Layer
consumeLayer w h = do
  rs <- (State.replicateM h $ consumeRow w)
  return $ Layer rs h

consumeImage :: Width -> Height -> State Stack Image
consumeImage w h = build 1 id
  where
    build :: Depth -> ([Layer] -> [Layer]) -> State Stack Image
    build d ls = do
      empty <- nullStack
      if empty then do
        return $ Image (ls []) d
      else do
        l <- consumeLayer w h
        build (d +1) (ls . (l:))

serialPixel :: ReadP Pixel
serialPixel = do
  ReadP.skipSpaces
  p <- ReadP.satisfy (\c -> any (c==) "012")
  return . Pixel . read $ [p]

spaceImage :: ReadP [Pixel]
spaceImage = do
  img <- ReadP.many serialPixel
  ReadP.skipSpaces
  ReadP.eof
  return img


loadSpaceImage :: Width -> Height -> String -> Image
loadSpaceImage w h ipt = 
  let
    ((flat, ""):[]) = ReadP.readP_to_S spaceImage ipt
    img = State.evalState (consumeImage w h) flat
  in img

type DigitCount = IntMap Int

countDigits :: Image -> [DigitCount]
countDigits img = fmap countLayer $ layers img
  where
    countLayer :: Layer -> DigitCount
    countLayer l = foldr countRow (IntMap.fromList [(0, 0), (1, 0), (2, 0)])$ rows l

    countRow :: Row -> DigitCount -> DigitCount
    countRow r accum = foldr (IntMap.adjust (1+) . Lib.toInteger) accum $ pixels r

checksum :: Image -> Int
checksum img =
  let
    counts = countDigits img
    leastZeros = ListKey.minimum (IntMap.! 0) counts
    ones = leastZeros IntMap.! 1
    twos = leastZeros IntMap.! 2
  in ones * twos

combinePixels :: Pixel -> Pixel -> Pixel
combinePixels new@(Pixel _) (Pixel 2) = new
combinePixels (Pixel _) old@(Pixel _) = old

linearAddress :: (Width, Height) -> Int -> Int -> Int
linearAddress (w, _) x y = y*w + x

dimensions :: Image -> (Width, Height)
dimensions img =
  let
    (layer:_) = layers img
    (row:_)   = rows layer
  in (rowWidth row, layerHeight layer)

-- Flatten image into a list of cooridnates and pixel.
-- Image is walks front-to-back in terms of layers
everyPixel :: Image -> [(Int, Int, Pixel)]
everyPixel img = concat $ fmap flattenLayer (layers img)
  where
    flattenLayer :: Layer -> [(Int, Int, Pixel)]
    flattenLayer = concat . fmap flattenRow . zip[0..] . rows
    flattenRow :: (Int, Row) -> [(Int, Int, Pixel)]
    flattenRow (y, row) = fmap (\(x,p) -> (x, y, p)) . zip [0..] . pixels $ row

rasterise :: Image -> RasterImage
rasterise img =
  let
    rasterMap = List.foldl' upsert IntMap.empty $ everyPixel img
    (w, h) = (dimensions img)
  in RasterImage $ buildLayer rasterMap w h

  where 
    convPos :: Int -> Int -> Int
    convPos = linearAddress (dimensions img)

    upsert :: IntMap Pixel -> (Int, Int, Pixel) -> IntMap Pixel
    upsert rast (x, y, p) = IntMap.insertWith combinePixels (convPos x y) p rast

    buildRow :: IntMap Pixel -> Int -> Width -> Row
    buildRow rast y w = Row (fmap (\x -> rast IntMap.! (convPos x y)) [0..(w-1)]) w

    buildLayer :: IntMap Pixel -> Width -> Height -> Layer
    buildLayer rast w h = Layer (fmap (\y -> buildRow rast y w) [0..(h-1)]) h

join :: String -> [String] -> String
join delim elems = concat $ intersperse elems
  where
    intersperse :: [String] -> [String]
    intersperse [] = []
    intersperse (e:es) = e:delim:(intersperse es)

render :: RasterImage -> String
render (RasterImage layer) = join "\n" $ fmap (\r -> fmap pixelToChar (pixels r)) (rows layer)
  where
    pixelToChar :: Pixel -> Char
    pixelToChar (Pixel 0) = ' '
    pixelToChar (Pixel 1) = '#'
    pixelToChar (Pixel 2) = 'O'
    pixelToChar (Pixel _) = error "Unknown pixel type"
