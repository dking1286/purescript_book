module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, foldl, null, (..))
import Data.Array.Partial (head, tail)
import Data.Foldable (product)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

countEvens :: Array Int -> Int
countEvens [] = 0
countEvens ints =
  step + countEvens (unsafePartial tail ints)
  where
    step = if isEven $ unsafePartial head $ ints then 1 else 0

square :: Int -> Int
square n = n * n

squares :: Array Int -> Array Int
squares = map square

isPositive :: Int -> Boolean
isPositive n = n >= 0

removeNegatives :: Array Int -> Array Int
removeNegatives = filter isPositive

infix 2 filter as <$?>

pairs :: Int -> Array (Array Int)
pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)

factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs n)

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

-- | 4.11 1
isPrime :: Int -> Boolean
isPrime = eq 1 <<< length <<< factors'

-- | 4.11 2
cartesianProduct :: forall a b. Array a -> Array b -> Array (Tuple a b)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure $ Tuple x y

-- | 4.11 3
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ square a + square b == square c
  pure [a, b, c]

-- | 4.15 3
count :: forall a. (a -> Boolean) -> Array a -> Int
count = count' 0
  where
    count' :: Int -> (a -> Boolean) -> Array a -> Int
    count' n _ [] = n
    count' n p xs = if p (unsafePartial head xs)
                        then count' (n + 1) p (unsafePartial tail xs)
                        else count' n p (unsafePartial tail xs)

-- | 4.15 4
reverse :: forall a. Array a -> Array a
reverse = foldl (\acc x -> [x] <> acc) []