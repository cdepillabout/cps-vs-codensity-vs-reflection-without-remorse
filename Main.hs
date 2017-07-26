{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad ((>=>), ap)

------------------
-- Normal style --
------------------

data FooM i o a
  = Await (i -> FooM i o a)
  | Yield o (FooM i o a)
  | Done a
  deriving Functor

awaitFooM :: FooM i o i
awaitFooM = Await Done

yieldFooM :: o -> FooM i o ()
yieldFooM o = Yield o (Done ())

instance Applicative (FooM i o) where
  pure :: a -> FooM i o a
  pure = Done

  (<*>) = ap

instance Monad (FooM i o) where
  (>>=) :: FooM i o a -> (a -> FooM i o b) -> FooM i o b
  Await icont >>= next = Await (\i -> icont i >>= next)
  Yield o cont >>= next = Yield o (cont >>= next)
  Done a >>= next = next a

interpFooM :: [i] -> FooM i o a -> ([o], a)
interpFooM inputs fooM = go inputs [] fooM
  where
    go :: [i] -> [o] -> FooM i o a -> ([o], a)
    go (i:is) os (Await next) = go is os (next i)
    go [] _os (Await _next) = error "not enough input!!"
    go is os (Yield o next) = go is (o:os) next
    go _is os (Done a) = (os, a)

myFooM :: FooM Int String Char
myFooM = do
  x <- awaitFooM
  yieldFooM . show $ x + 3
  yieldFooM . show $ x + 6
  pure . head . show $ x + 10

testFooM :: ([String], Char)
testFooM = interpFooM [1..] myFooM

---------
-- CPS --
---------

newtype FooCPS i o a = FooCPS
  { runFooCPS
      :: forall r.
         ((i -> FooCPS i o a) -> r)
      -> (o -> FooCPS i o a -> r)
      -> (a -> r)
      -> r
  } deriving Functor

awaitFooCPS :: FooCPS i o i
awaitFooCPS = FooCPS $ \awaitCont _ _ ->
  awaitCont $ \i -> FooCPS $ \_ _ doneCont -> doneCont i

yieldFooCPS :: o -> FooCPS i o ()
yieldFooCPS o = FooCPS $ \_ yieldCont _ ->
  yieldCont o $ FooCPS $ \_ _ doneCont -> doneCont ()

instance Applicative (FooCPS i o) where
  pure :: a -> FooCPS i o a
  pure a = FooCPS $ \_ _ doneCont -> doneCont a

  (<*>) = ap

instance Monad (FooCPS i o) where
  (>>=) :: forall a b. FooCPS i o a -> (a -> FooCPS i o b) -> FooCPS i o b
  (FooCPS f) >>= next = FooCPS $ \awaitCont yieldCont doneCont ->
    f
      (\fooCPSA -> awaitCont $ \i -> fooCPSA i >>= next)
      (\o fooCPSA -> yieldCont o $ fooCPSA >>= next)
      (\a ->
        let FooCPS g = next a
        in g awaitCont yieldCont doneCont
      )

interpFooCPS :: [i] -> FooCPS i o a -> ([o], a)
interpFooCPS inputs fooCPS = go inputs [] fooCPS
  where
    go :: [i] -> [o] -> FooCPS i o a -> ([o], a)
    go is os (FooCPS f) =
      f
        (\next ->
          case is of
            [] -> error "not enough input!!"
            (i:is') -> go is' os (next i)
        )
        (\o next -> go is (o:os) next)
        (\a -> (os, a))

myFooCPS :: FooCPS Int String Char
myFooCPS = do
  x <- awaitFooCPS
  yieldFooCPS . show $ x + 3
  yieldFooCPS . show $ x + 6
  pure . head . show $ x + 10

testFooCPS :: ([String], Char)
testFooCPS = interpFooCPS [1..] myFooCPS

---------------
-- Codensity --
---------------

newtype Codensity m a = Codensity
  { runCodensity :: forall b. (a -> m b) -> m b
  } deriving Functor

instance Applicative (Codensity m) where
  pure :: a -> Codensity m a
  pure a = Codensity $ \f -> f a

  (<*>) = ap

instance Monad (Codensity m) where
  (>>=) :: forall a b. Codensity m a -> (a -> Codensity m b) -> Codensity m b
  Codensity f >>= next =
    Codensity $ \btomy -> f $ \a -> (runCodensity $ next a) btomy

newtype FooCodensity i o a = FooCodensity
  { runFooCodensity :: Codensity (FooM i o) a
  } deriving (Functor, Applicative, Monad)

fooMToFooCodensity :: forall i o a. FooM i o a -> FooCodensity i o a
fooMToFooCodensity (Done a) =
  FooCodensity $ Codensity $ \f -> f a
fooMToFooCodensity (Yield o next) =
  FooCodensity $ Codensity $ \f -> Yield o (next >>= f)
fooMToFooCodensity (Await inext) =
  FooCodensity $ Codensity $ \f -> Await (\i -> inext i >>= f)

fooCodensityToFooM :: forall i o a. FooCodensity i o a -> FooM i o a
fooCodensityToFooM (FooCodensity (Codensity f)) = f Done

awaitFooCodensity :: FooCodensity i o i
awaitFooCodensity =
  FooCodensity $
    Codensity $
      \itomb -> Await (\i -> itomb i)

yieldFooCodensity :: o -> FooCodensity i o ()
yieldFooCodensity o =
  FooCodensity $
    Codensity $
      \unittomb -> Yield o (unittomb ())

interpFooCodensity :: forall i o a. [i] -> FooCodensity i o a -> ([o], a)
interpFooCodensity inputs fooCodensity =
  interpFooM inputs $ fooCodensityToFooM fooCodensity

myFooCodensity :: FooCodensity Int String Char
myFooCodensity = do
  x <- awaitFooCodensity
  yieldFooCodensity . show $ x + 3
  yieldFooCodensity . show $ x + 6
  pure . head . show $ x + 10

testFooCodensity :: ([String], Char)
testFooCodensity = interpFooCodensity [1..] myFooCodensity

--------------------------------
-- Reflection Without Remorse --
--------------------------------

type FooExplicit i o = FTCQueue (FooRWR i o)

data FooRWR i o a
  = AwaitRWR (forall x. i -> FooExplicit i o x a)
  | YieldRWR  o (forall x. FooExplicit i o x a)
  | DoneRWR a

instance Functor (FooRWR i o) where
  fmap :: (a -> b) -> FooRWR i o a -> FooRWR i o b
  fmap f (AwaitRWR inext) = AwaitRWR (\i -> inext i |> DoneRWR . f)
  fmap f (YieldRWR o next) = YieldRWR o (next |> DoneRWR . f)
  fmap f (DoneRWR a) = DoneRWR $ f a

instance Applicative (FooRWR i o) where
  pure :: a -> FooRWR i o a
  pure a = DoneRWR a

  (<*>) = ap

instance Monad (FooRWR i o) where
  (>>=) :: FooRWR i o a -> (a -> FooRWR i o b) -> FooRWR i o b
  AwaitRWR inext >>= kont = AwaitRWR (\i -> inext i |> kont)
  YieldRWR o next >>= kont = YieldRWR o (next |> kont)
  DoneRWR a >>= kont = kont a

awaitFooRWR :: FooRWR i o i
awaitFooRWR = AwaitRWR $ \i -> Leaf $ \_ -> DoneRWR i

yieldFooRWR :: o -> FooRWR i o ()
yieldFooRWR o = YieldRWR o $ Leaf $ \_ -> DoneRWR ()

-- | Function application in the context @FooExplicit@
queueApply :: FooExplicit i o a b -> a -> FooRWR i o b
queueApply fooExplicit a =
  case tviewl fooExplicit of
    TOne k -> k a
    k :| t ->
      case k a of
        DoneRWR x -> queueApply t x
        YieldRWR o next -> YieldRWR o (Node next t)
        AwaitRWR inext -> AwaitRWR (\i -> Node (inext i) t)

interpFooRWR :: forall i o a. [i] -> FooRWR i o a -> ([o], a)
interpFooRWR inputs fooRWR = go inputs [] fooRWR
  where
    go :: [i] -> [o] -> FooRWR i o a -> ([o], a)
    go [] _ (AwaitRWR _) = error "not enough input!!"
    go (i:is) os (AwaitRWR inext) =
      go is os $ queueApply (inext i) ()
    go is os (YieldRWR o next) =
      go is (o:os) $ queueApply next ()
    go _is os (DoneRWR a) = (os, a)

myFooRWR :: FooRWR Int String Char
myFooRWR = do
  x <- awaitFooRWR
  yieldFooRWR . show $ x + 3
  yieldFooRWR . show $ x + 6
  pure . head . show $ x + 10

testFooRWR :: ([String], Char)
testFooRWR = interpFooRWR [1..] myFooRWR

----------------------------------------------
-- Performance Problem Caused by Deep Binds --
----------------------------------------------

-- | Adapted from the Reflection Without Remorse paper:
--
-- This function uses 'Await' to sum an input list of numbers.
--
-- 'FooM' and 'FooCPS' exhibit quadratic running time  because of
-- left-associated binds.  'FooCodensity' and 'FooRWR' do not have this
-- problem.
--
-- For instance, these will be slow:
--
-- @
--   interpFooM [1..] $ sumInputFooM 10000
-- @

-- @
--   interpFooCPS [1..] $ sumInputFooCPS 10000
-- @
--
-- These should be fast:
--
-- @
--   interpFooCodensity [1..] $ sumInputFooCodensity 10000
-- @
--
-- @
--   interpFooRWR [1..] $ sumInputFooRWR 10000
-- @
--
-- Here is the (simpler) example from Reflection Without Remorse:
--
-- > data It i a = Get (i -> It i a) | Done a
-- >
-- > sumInput :: Int -> It Int Int
-- > sumInput n = Get (foldl (>=>) return (replicate (n - 1) f))
-- >   where
-- >     f x = get >>= return . (+ x)
sumInputFooM :: forall o. Int -> FooM Int o Int
sumInputFooM n =
  Await $ \i -> foldl (>=>) pure (replicate (n - 1) f) i
  where
    f :: Int -> FooM Int o Int
    f x = awaitFooM >>= pure . (+ x)

sumInputFooCPS :: Int -> FooCPS Int o Int
sumInputFooCPS n = FooCPS $ \awaitCont _ _ ->
  awaitCont $ \i -> foldl (>=>) pure (replicate (n - 1) f) i
  where
    f :: Int -> FooCPS Int o Int
    f x = awaitFooCPS >>= pure . (+ x)

sumInputFooCodensity :: Int -> FooCodensity Int o Int
sumInputFooCodensity n = FooCodensity $ Codensity $ \cont ->
  Await $ \i ->
    let (FooCodensity (Codensity inner)) =
          foldl (>=>) pure (replicate (n - 1) f) i
    in inner cont
  where
    f :: Int -> FooCodensity Int o Int
    f x = awaitFooCodensity >>= pure . (+ x)

sumInputFooRWR :: forall o. Int -> FooRWR Int o Int
sumInputFooRWR n =
  AwaitRWR $ \i -> Leaf $ \_ -> foldl (>=>) pure (replicate (n - 1) f) i
  where
    f :: Int -> FooRWR Int o Int
    f x = awaitFooRWR >>= pure . (+ x)

------------------------
-- Reflection Example --
------------------------

-- | Adapted from the Reflection Without Remorse paper:
--
-- This 'par' operator runs both 'FooM' coroutines in parallel. When one of the
-- input 'FooM' is 'Done', the remaining computation of both arguments is
-- returned, along with any elements that have been 'Yield'.  When both
-- arguments are 'Await', the outer 'FooM' monad will wait for one input and
-- pass it to both argument 'FooM'.
--
-- This 'par' function is an instance of monad reflection: we observe the
-- internal state of both 'FooM'.  It is not possible to implement this on
-- 'FooCPS' or 'FooCodensity' without first converting it back to something
-- like 'FooM'.  It is, however, possible to implement this with 'FooRWR'.
--
-- Here is the (simpler) example from Reflection Without Remorse:
--
-- > data It i a = Get (i -> It i a) | Done a
-- >
-- > par :: It i a -> It i b -> It i (It i a, It i b)
-- > par l r
-- >   | Done <- l = Done (l, r)
-- >   | Done <- r = Done (l, r)
-- >   | Get f <- l, Get g <- r = Get Done >>= \x -> par (f x) (g x)
par :: FooM i o a -> FooM i o b -> FooM i o (FooM i o a, [o], FooM i o b, [o])
par l r = go l [] r []
  where
    go
      :: FooM i o a -- ^ Input 'FooM' called @left@.
      -> [o] -- ^ Elements that have been output by @left@.
      -> FooM i o b -- ^ Input 'FooM' called @right@.
      -> [o] -- ^ Elements that have been output by @right@.
      -> FooM i o (FooM i o a, [o], FooM i o b, [o])
    go left leftOs right rightOs
      | Done _ <- left = Done (left, leftOs, right, rightOs)
      | Done _ <- right = Done (left, leftOs, right, rightOs)
      | Await leftNext <- left, Await rightNext <- right = do
          i <- awaitFooM
          go (leftNext i) leftOs (rightNext i) rightOs
      | Yield leftO leftNext <- left, Yield rightO rightNext <- right =
          go leftNext (leftO : leftOs) rightNext (rightO : rightOs)
      | Yield leftO leftNext <- left =
          go leftNext (leftO : leftOs) right rightOs
      | Yield rightO rightNext <- right =
          go left leftOs rightNext (rightO : rightOs)

---------------------------------------------------
-- FTCQueue (Used by Reflection-Without-Remorse) --
---------------------------------------------------

-- The following code was adapted from
-- https://hackage.haskell.org/package/freer-effects-0.3.0.1/docs/src/Data-FTCQueue.html

-- | Non-empty tree. Deconstruction operations make it more and more
-- left-leaning
data FTCQueue m a b where
  Leaf :: (a -> m b) -> FTCQueue m a b
  Node :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b

-- | Append an operation to the right of the tree. [O(1)]
(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
t |> r = Node t (Leaf r)
{-# INLINE (|>) #-}
infixl 8 |>

-- | Left view deconstruction data structure.
data ViewL m a b where
  TOne :: (a -> m b) -> ViewL m a b
  (:|) :: (a -> m x) -> FTCQueue m x b -> ViewL m a b

-- | Left view deconstruction. [average O(1)]
tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf r)     = TOne r
tviewl (Node t1 t2) = go t1 t2
  where
    go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
    go (Leaf r)       tr = r :| tr
    go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)

----------
-- Main --
----------

main :: IO ()
main = do
  putStrLn "fooM:"
  print testFooM
  putStrLn "\nfooCPS:"
  print testFooCPS
  putStrLn "\nfooCodensity:"
  print testFooCodensity
  putStrLn "\nfooRWR:"
  print testFooRWR
