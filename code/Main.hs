module Main where

import Common (D, G, I, T, Var (..), W, c0, negD, showI, trueD)
import Control.Arrow (first, second, (>>>))
import Control.Monad ((>=>))
import DPL qualified
import Data.Map qualified as M
import Text.Pretty.Simple (pPrint)

class DN repr where
  up :: D -> repr
  down :: repr -> D
  -- down . up == id
  -- up . down /= id
  neg :: repr -> repr
  -- neg . neg == id
  disp :: repr -> IO ()
-- convenience function for displaying / pretty-printing

p :: DN repr => Var -> repr
p = up . DPL.p
-- true of 'a' @1 and 'b' @2

p1 :: DN repr => Var -> repr
p1 = up . DPL.p1
-- true of 'a' and 'b' @1 and nothing @2

top :: DN repr => Var -> repr
top = up . DPL.top

r :: DN repr => Var -> Var -> repr
r = (up .) . DPL.r
-- true of ('b','a') @1 and ('a','b') @2

eq :: DN repr => Var -> Var -> repr
eq = (up .) . DPL.eq
-- equality

ex :: DN repr => Var -> repr -> repr
ex v = up . DPL.ex v . down
-- existential quantification

upCon :: DN repr => (D -> D -> D) -> repr -> repr -> repr
upCon f l r = up $ f (down l) (down r)
-- lifting binary connectives

(/\) :: DN repr => repr -> repr -> repr
(/\) = upCon (DPL./\)
-- lifted conjunction

(\/) :: DN repr => repr -> repr -> repr
l \/ r = up $ down l <> down (neg l /\ r)
--                        cf. neg l ~> r
-- Internally dynamic disjunction (since any DN repr's neg is involutive).
-- Choice of /\ over ~> means we generate the weak reading.
--
-- Use of <> (here, p <> q = \i -> p i ++ q i) means that disjunction is also
-- externally dynamic, like G&S's Program Disjunction (here, only dynamic if
-- dref introduced @ same index in l and r).
--
-- /= neg (neg l /\ neg r)
-- /= upCon (DPL.\/)

(~>) :: DN repr => repr -> repr -> repr
(~>) = upCon (DPL.~>)
-- lifted material conditional

s0 :: DN repr => repr
s0 = ex X (p1 X)

s1 :: DN repr => repr
s1 = neg s0

s2 :: DN repr => repr
s2 = neg $ neg s0

s3 :: DN repr => repr
s3 = neg s0 \/ p X

s4 :: DN repr => repr
s4 = s0 \/ ex X (neg $ p1 X)

s5 :: DN repr => repr
s5 = s0 \/ ex Y (neg $ p1 Y)

suite :: DN repr => [repr]
suite = [s0, s1, s2, s3, s4, s5]

-- ** (I -> [I], I -> [I]) **
--
-- Krahmer & Muskens: meanings are pairs of positive and negative updates.
-- Negation swaps dimensions. Isomorphic to I -> O^2.
--
data Squ = Squ {positive :: I -> [I], negative :: I -> [I]}

instance DN Squ where
  up :: D -> Squ
  up phi = Squ phi (negD phi)
  down :: Squ -> D
  down (Squ a _) = a
  --
  neg :: Squ -> Squ
  neg (Squ a b) = Squ b a
  --
  disp :: Squ -> IO ()
  disp (Squ a b) = pPrint $ both DPL.showD (a, b)
    where
      both f = first f >>> second f

-- ** I -> ([I], T) **
--
-- The only (I -> O, I -> O) pairs K&M's system generates are contraries (as
-- enforced here by Squ's `up` and `neg` maps). This restriction means we can
-- replace O^2 with O*2. We might think of this as staging DPL updates, by
-- flagging outputs for elimination or retention. Negation toggles this flag.
-- Executing the update happens via an `up . down` roundtrip, whereas in DPL,
-- the execution/elimination/retention happens automatically.
--
newtype Tag = Tag {runTag :: I -> ([I], T)}

instance DN Tag where
  up :: D -> Tag
  up phi =
    Tag \i ->
      (phi <> negD phi $ i, trueD i phi)
  -- == squTag . up == monTag . up
  down :: Tag -> D
  down (Tag f) i = concat [fst $ f i | snd $ f i]
  -- == down . tagSqu == down . tagMon
  --
  neg :: Tag -> Tag
  neg (Tag m) = Tag $ fmap not . m
  -- == squTag . neg . tagSqu == monTag . neg . tagMon
  --
  disp :: Tag -> IO ()
  disp (Tag m) = pPrint $ map (first (map showI) . m) c0

tagSqu :: Tag -> Squ
tagSqu (Tag f) = Squ a b
  where
    proto = fst . f
    cond = snd . f
    a i = concat [proto i | cond i]
    b i = concat [proto i | not $ cond i]

squTag :: Squ -> Tag
squTag (Squ a b) =
  Tag \i ->
    (a <> b $ i, trueD i a)
-- sqTag . tagSqu == id
-- prove: (tagSqu . squTag) (a,b) == (a,b), when a and b are contraries

-- ** (I -> T, I -> [I]) **
-- An equivalent but more intuitive representation of Tag
data Stage = Stage {static :: I -> T, protoDyn :: I -> [I]}

instance DN Stage where
  up :: D -> Stage
  up phi = Stage (`trueD` phi) (phi <> negD phi)
  down :: Stage -> D
  down (Stage tc f) i = [j | tc i, j <- f i]
  --
  neg :: Stage -> Stage
  neg (Stage tc f) = Stage (not . tc) f
  --
  disp :: Stage -> IO ()
  disp (Stage tc f) = pPrint ([showI i | i <- c0, tc i], DPL.showD $ \i -> f i)

stageSqu :: Stage -> Squ
stageSqu (Stage tc f) = Squ a b
  where
    a i = [j | tc i, j <- f i]
    b i = [j | not $ tc i, j <- f i]

squStage :: Squ -> Stage
squStage (Squ a b) = Stage tc f
  where
    tc i = trueD i a
    f = a <> b

-- ** I -> [(I, T)] **
--
-- Monadic dynamic semantics. Mon T >> I -> ([I], T). The difference btw Mon T
-- and Tag is the ability to countenance 'negative/falsifying witnesses' at
-- an index where the sentence is True. This is helpful for exceptional scope
-- (and does not need to be given up here, as indefinites can always evade
-- negation -- see below), but otiose for double negation.
--
newtype Mon a = Mon {runMon :: I -> [(I, a)]}
  deriving (Functor)

instance Applicative Mon where
  pure :: a -> Mon a
  pure x = Mon \i -> pure (i, x)
  (<*>) :: Mon (a -> b) -> Mon a -> Mon b
  m <*> n = m >>= \f -> n >>= \x -> return $ f x

instance Monad Mon where
  return :: a -> Mon a
  return = pure
  (>>=) :: Mon a -> (a -> Mon b) -> Mon b
  Mon m >>= f = Mon $ m >=> uncurry (flip $ runMon . f)

trueMon :: I -> Mon T -> T
trueMon i (Mon m) = any snd (m i)

homogenize :: Mon T -> Mon T
homogenize m@(Mon f) =
  Mon \i ->
    if trueMon i m then filter snd (f i) else f i
-- codomain of `homogenize` is isomorphic to Tag

instance DN (Mon T) where
  up :: D -> Mon T
  up phi =
    Mon \i ->
      let b = trueD i phi
       in map
            (,b)
            if b then phi i else [i]
  -- == tagMon . up == squMon . up
  down :: Mon T -> D
  down (Mon f) i = [j | (j, b) <- f i, b]
  -- == down . monTag == down . monSqu
  --
  neg :: Mon T -> Mon T
  neg = fmap not -- . homogenize
  -- == tagMon . neg . monTag == squMon . neg . monSqu
  disp :: Mon T -> IO ()
  disp (Mon f) = pPrint $ c0 >>= map (first showI) . f

tagMon :: Tag -> Mon T -- injective
tagMon (Tag f) =
  Mon \i ->
    [(j, b) | let (o, b) = f i, j <- o]

monTag :: Mon T -> Tag -- lossy
monTag m@(Mon f) =
  Tag
    \i ->
      let b = trueMon i m
       in ([j | (j, b') <- f i, b == b'], b)

squMon :: Squ -> Mon T -- injective
squMon = tagMon . squTag

monSqu :: Mon T -> Squ -- lossy
monSqu = tagSqu . monTag

exR :: Var -> Mon T -> Mon T -> Mon T
exR v r s =
  Mon \i ->
    let rUpd = ex v r
     in if trueD i (down rUpd)
          then concat [runMon s j | (j, True) <- runMon rUpd i]
          else [(i, False)]
-- exR generates negative witnesses, which disappear with
-- `down`, `monSqu`, or `monTag`

s6 :: Mon T
s6 = exR X (p1 X) (p X)

s7 :: Mon T
s7 = s6 /\ p X
-- /\ induces `up (...) down` round-trip, so neg witnesses disappear

s8 :: Mon T
s8 = do
  b <- s6
  return b /\ p X
-- neg witnesses retained with monadic wide scope

test :: DN repr => [repr] -> IO ()
test = mapM_ disp

main :: IO ()
main = do
  test @Squ suite
  putStrLn "\n\n***********\n\n"
  test @Tag suite
  putStrLn "\n\n***********\n\n"
  test @(Mon T) $ suite
  putStrLn "\n\n***********\n\n"
  test [s6, s7, s8]
