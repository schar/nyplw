module DPL where

import Common (D, E, T, Var (..), W, c0, dom, showI)
import Data.Char (ord)
import Data.Map qualified as M
import Data.Maybe (fromJust)

upgradeP :: (W -> E -> T) -> Var -> D
upgradeP f v i@(g, w) = [i | f w $ fromJust $ M.lookup v g]

upgradeR :: (W -> E -> E -> T) -> Var -> Var -> D
upgradeR f u v i@(g, w) = [i | uncurry (f w) $ fromJust mp]
  where
    mp = (,) <$> M.lookup u g <*> M.lookup v g

p :: Var -> D
p = upgradeP p'
  where
    p' w
      | even w = even . ord
      | otherwise = odd . ord

p1 :: Var -> D
p1 = upgradeP $ \w !_ -> w == 1

top :: Var -> D
top = upgradeP $ const f
  where
    f !_ = True

r :: Var -> Var -> D
r = upgradeR r'
  where
    r' w x y
      | even w = x < y
      | otherwise = x > y

eq :: Var -> Var -> D
eq = upgradeR $ const (==)

ex :: Var -> D -> D
ex v phi (g, w) = do
  d <- dom
  phi (M.insert v d g, w)

neg :: D -> D
neg phi i = [i | null (phi i)]

(/\) :: D -> D -> D
(l /\ r) g = do
  h <- l g
  r h

(\/) :: D -> D -> D
l \/ r = l <> (neg l /\ r)

(~>) :: D -> D -> D
l ~> r = neg (l /\ neg r)

s0 :: D
s0 = neg $ neg $ ex X (p1 X)

s1 :: D
s1 = neg s0 \/ p X

s2 :: D
s2 = ex X (p1 X) \/ ex X (neg $ p1 X)

s3 :: D
s3 = ex X (p X) \/ ex X (neg $ p X)

s4 :: D
s4 = ex X (p1 X) \/ ex Y (neg $ p1 Y)

suite :: [D]
suite = [s0, s1, s2, s3, s4]

showD :: D -> [String]
showD phi = c0 >>= \i -> return $ showI i ++ " -> " ++ show (map showI (phi i))
