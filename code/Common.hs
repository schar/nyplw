module Common where

import Data.List (intercalate)
import Data.Map qualified as M

data Var = X | Y
  deriving (Show, Eq, Enum, Ord)

type E = Char
type T = Bool
type G = M.Map Var E
type W = Int

type I = (G, W)
type O = [I]
type D = I -> O

dom :: [E]
dom = ['a' .. 'b']

ws :: [W]
ws = [1 .. 2]

c0 :: O
c0 = [(M.empty, w) | w <- ws]

trueD :: I -> D -> Bool
trueD i phi = phi i /= []

negD :: D -> D
negD phi i = [i | not $ trueD i phi]

showI :: I -> String
showI (g, w) = intercalate ", " (showG g) ++ ", " ++ "w" ++ show w

showG :: G -> [String]
showG g
  | g == M.empty = ["<null>"]
  | otherwise = map pairArr $ M.toList g
  where
    pairArr (var, e) = show var ++ "/" ++ [e]
