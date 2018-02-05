{-# OPTIONS_GHC -O2 #-}

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS

import Control.Monad
import Data.Functor
import Data.Int (Int64)
import Data.Ord
import Data.List
import System.Random

gauss :: Double -> IO Double
gauss scale = do
  x1 <- randomIO
  x2 <- randomIO
  return $ scale * sqrt (-2 * log x1) * cos (2 * pi * x2)

newBrain :: [Int] -> IO [([Double], [[Double]])]
newBrain szs@(_:ts) = zip (flip replicate 1 <$> ts) <$>
  zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) szs ts

relu :: Double -> Double
relu = max 0

relu' :: (Ord a, Num a, Num b) => a -> b
relu' x | x < 0      = 0
        | otherwise  = 1

zLayer :: Num a => [a] -> ([a], [[a]]) -> [a]
zLayer as (bs, wvs) = zipWith (+) bs $ sum . zipWith (*) as <$> wvs

feed :: [Double] -> [([Double], [[Double]])] -> [Double]
feed = foldl' (((relu <$>) . ) . zLayer)

revaz :: Foldable t => [Double] -> t ([Double], [[Double]]) -> ([[Double]], [[Double]])
revaz xs = foldl' (\(avs@(av:_), zs) (bs, wms) -> let
  zs' = zLayer av (bs, wms) in ((relu <$> zs'):avs, zs':zs)) ([xs], [])

dCost :: (Num a, Ord a) => a -> a -> a
dCost a y | y == 1 && a >= y = 0
          | otherwise        = a - y

deltas :: [Double] -> [Double] -> [([Double], [[Double]])] -> ([[Double]], [[Double]])
deltas xv yv layers = let
  (avs@(av:_), zv:zvs) = revaz xv layers
  delta0 = zipWith (*) (zipWith dCost av yv) (relu' <$> zv)
  in (reverse avs, f (transpose . snd <$> reverse layers) zvs [delta0]) where
    f _ [] dvs = dvs
    f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
      zipWith (*) [sum $ zipWith (*) row dv | row <- wm] (relu' <$> zv)

eta :: Double
eta = 0.002

descend :: [Double] -> [Double] -> [Double]
descend av dv = zipWith (-) av ((eta *) <$> dv)

learn :: [Double] -> [Double] -> [([Double], [[Double]])] -> [([Double], [[Double]])]
learn xv yv layers = let (avs, dvs) = deltas xv yv layers
  in zip (zipWith descend (fst <$> layers) dvs) $
    zipWith3 (\wvs av dv -> zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
      (snd <$> layers) avs dvs

getImage s n = fromIntegral . BS.index s . (n*28^2 + 16 +) <$> [0..28^2 - 1]
getX     s n = (/ 256) <$> getImage s n
getLabel s n = fromIntegral $ BS.index s (n + 8)
getY     s n = fromIntegral . fromEnum . (getLabel s n ==) <$> [0..9]

render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)

main :: IO ()
main = do
  [trainI, trainL, testI, testL] <- mapM ((decompress  <$>) . BS.readFile)
    [ "train/train-images-idx3-ubyte.gz"
    , "train/train-labels-idx1-ubyte.gz"
    , "train/t10k-images-idx3-ubyte.gz"
    , "train/t10k-labels-idx1-ubyte.gz"
    ]
  b <- newBrain [784, 30, 10]
  n <- (`mod` 10000) <$> randomIO
  putStr . unlines $
    take 28 $ take 28 <$> iterate (drop 28) (render <$> getImage testI n)

  let
    example = getX testI n
    bs = scanl (foldl' (\b n -> learn (getX trainI n) (getY trainL n) b)) b [
     [   0.. 999],
     [1000..2999],
     [3000..5999],
     [6000..9999]]
    smart = last bs
    cute d score = show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'
    bestOf = fst . maximumBy (comparing snd) . zip [0..]

  forM_ bs $ putStrLn . unlines . zipWith cute [0..9] . feed example

  putStrLn $ "best guess: " ++ show (bestOf $ feed example smart)

  let guesses = bestOf . (\n -> feed (getX testI n) smart) <$> [0..9999]
  let answers = getLabel testL <$> [0..9999]
  putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++
    " / 10000"
