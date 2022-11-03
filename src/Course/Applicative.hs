{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Applicative where

import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P(fmap, pure, (>>=))

-- | All instances of the `Applicative` type-class must satisfy four laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. pure id <*> x = x`
--
-- * The law of composition
--   `∀u v w. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
--
-- * The law of homomorphism
--   `∀f x. pure f <*> pure x = pure (f x)`
--
-- * The law of interchange
--   `∀u y. u <*> pure y = pure ($ y) <*> u`

class Functor k => Applicative k where
  pure ::
    a -> k a
  -- Pronounced, apply.
  (<*>) ::
    k (a -> b)
    -> k a
    -> k b

infixl 4 <*>

-- | Insert into ExactlyOne.
--
-- prop> \x -> pure x == ExactlyOne x
--
-- >>> ExactlyOne (+10) <*> ExactlyOne 8
-- ExactlyOne 18
instance Applicative ExactlyOne where
  pure ::
    a
    -> ExactlyOne a
  pure a = ExactlyOne a
    -- error "todo: Course.Applicative pure#instance ExactlyOne"
  (<*>) ::
    ExactlyOne (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  -- (<*>) (ExactlyOne f) (ExactlyOne a) = ExactlyOne (f a)

  (<*>) (ExactlyOne fab) (ExactlyOne a) = pure (fab a)
    -- error "todo: Course.Applicative (<*>)#instance ExactlyOne"

-- | Insert into a List.
--
-- prop> \x -> pure x == x :. Nil
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance Applicative List where
  pure ::
    a
    -> List a
  pure a = a :. Nil
    -- error "todo: Course.Applicative pure#instance List"
  (<*>) ::
    List (a -> b)
    -> List a
    -> List b
  (<*>) Nil _ = Nil
  -- (<*>) (fab :. fabs) la = (fab <$> la) ++ (fabs <*> la)
  -- (<*>) Nil _ = Nil
  (<*>) xs ys = flatMap (<$> ys) xs
  -- (<*>) (x :. xs) ys = (x <$> ys) ++ (xs <*> ys)
    -- let this = (x <$> ys) in
    --   let next = (xs <*> ys) in
    --     this ++ next
    -- error "todo: Course.Apply (<*>)#instance List"

-- | Insert into an Optional.
--
-- prop> \x -> pure x == Full x
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance Applicative Optional where
  pure ::
    a
    -> Optional a
  pure = Full
    -- error "todo: Course.Applicative pure#instance Optional"
  (<*>) ::
    Optional (a -> b)
    -> Optional a
    -> Optional b
  (<*>) Empty _ = Empty
  (<*>) _ Empty = Empty
  (<*>) (Full fab) (Full a) = Full (fab a)
  -- (<*>) (Full fab) (Full a) = mapOptional (fab a)
  -- (<*>) = applyOptional 
    -- error "todo: Course.Apply (<*>)#instance Optional"

-- | Insert into a constant function.
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
--
-- prop> \x y -> pure x y == x
instance Applicative ((->) t) where
  -- pure ::
  --   a
  --   -> ((->) t a)
  pure ::
    a
    -> t
    -> a
  -- pure a t = a
  pure = const
    -- error "todo: Course.Applicative pure#((->) t)"
  (<*>) ::
    -- ((->) t (a -> b))
    -- -> ((->) t a)
    -- -> ((->) t b)
    (t -> a -> b)
    -> (t -> a)
    -> (t -> b)
  -- (<*>) fab ta t = fab t (ta t)
  -- (<*>) fab ta v = fab v (ta v)
  (<*>) tab ta v = tab v (ta v)
    -- error "todo: Course.Apply (<*>)#instance ((->) t)"


-- | Apply a binary function in the environment.
--
-- >>> lift2 (+) (ExactlyOne 7) (ExactlyOne 8)
-- ExactlyOne 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum (listh [4,5,6])
-- 18
lift2 ::
  Applicative k =>
  (a -> b -> c)
  -> k a
  -> k b
  -> k c
-- lift2 fabc fa fb = pure fabc <*> fa <*> fb
-- lift2 f a b = f <$> a <*> b
lift2 abc ka kb = lift1 abc ka <*> kb
  -- error "todo: Course.Applicative#lift2"

-- | Apply a ternary function in the environment.
-- /can be written using `lift2` and `(<*>)`./
--
-- >>> lift3 (\a b c -> a + b + c) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9)
-- ExactlyOne 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
-- 138
lift3 ::
  Applicative k =>
  (a -> b -> c -> d)
  -> k a
  -> k b
  -> k c
  -> k d
-- lift3 f ka kb kc = pure f <*> ka <*> kb <*> kc
lift3 f ka kb kc = lift2 f ka kb <*> kc
  -- error "todo: Course.Applicative#lift3"

-- | Apply a quaternary function in the environment.
-- /can be written using `lift3` and `(<*>)`./
--
-- >>> lift4 (\a b c d -> a + b + c + d) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9) (ExactlyOne 10)
-- ExactlyOne 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 ::
  Applicative k =>
  (a -> b -> c -> d -> e)
  -> k a
  -> k b
  -> k c
  -> k d
  -> k e
lift4 f ka kb kc kd = lift3 f ka kb kc <*> kd
  -- error "todo: Course.Applicative#lift4"

-- | Apply a nullary function in the environment.
lift0 ::
  Applicative k =>
  a
  -> k a
lift0 = pure
  -- error "todo: Course.Applicative#lift0"

-- | Apply a unary function in the environment.
-- /can be written using `lift0` and `(<*>)`./
--
-- >>> lift1 (+1) (ExactlyOne 2)
-- ExactlyOne 3
--
-- >>> lift1 (+1) Nil
-- []
--
-- >>> lift1 (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
lift1 ::
  -- Applicative k =>
  (Functor k, Applicative k) =>
  (a -> b)
  -> k a
  -> k b
-- lift1 f ka = lift0 f <*> ka
lift1 = (<$>)  -- use functor
  -- error "todo: Course.Applicative#lift1"

-- | Apply, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. Nil)
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> \a b c x y z -> (a :. b :. c :. Nil) *> (x :. y :. z :. Nil) == (x :. y :. z :. x :. y :. z :. x :. y :. z :. Nil)
--
-- prop> \x y -> Full x *> Full y == Full y
(*>) ::
  Applicative k =>
  k a
  -> k b
  -> k b
-- (*>) = lift2 (\m n -> n)
(*>) = lift2 (\_ b -> b)
  -- lift2 (flip const)

  -- error "todo: Course.Applicative#(*>)"

-- | Apply, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> (1 :. 2 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2]
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. Nil)
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> \x y z a b c -> (x :. y :. z :. Nil) <* (a :. b :. c :. Nil) == (x :. x :. x :. y :. y :. y :. z :. z :. z :. Nil)
--
-- prop> \x y -> Full x <* Full y == Full x
(<*) ::
  Applicative k =>
  k b
  -> k a
  -> k b
(<*) = lift2 const
  -- error "todo: Course.Applicative#(<*)"

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence ::
  Applicative k =>
  List (k a)
  -> k (List a)
sequence Nil = pure Nil
sequence (x:.xs) = lift2 (:.) x (sequence xs)
-- sequence (x:.xs) = lift2 (++) (lift1 (:. Nil) x) (sequence xs)
-- sequence (x:.xs) = lift2 (++) ((:. Nil) <$> x) (sequence xs)
-- sequence (x:.xs) = pure (++) <*> (pure (:. Nil) <*> x) <*> sequence xs
-- Have an empty list then iterate on the items on the list, lift them to turn them into a list and concat them
-- sequence = foldLeft (\acc b -> lift2 (++) acc (lift1 (:.Nil) b)) (pure Nil)
-- sequence (ka :. kas) = lift2 (\a as -> a :. as) ka (sequence kas)
-- sequence = foldRight (lift2 (:.)) (pure Nil)

  -- error "todo: Course.Applicative#sequence"

-- | Replicate an effect a given number of times.
--
-- /Tip:/ Use `Course.List#replicate`.
--
-- >>> replicateA 4 (ExactlyOne "hi")
-- ExactlyOne ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ('a' :. 'b' :. 'c' :. Nil)
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA ::
  Applicative k =>
  Int
  -> k a
  -> k (List a)
-- replicateA c k = sequence (replicate c k)
replicateA c = sequence . replicate c
-- replicateA c k = lift1 (\a -> replicate c a) k
-- replicateA c = lift1 (replicate c)

  -- error "todo: Course.Applicative#replicateA"

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (ExactlyOne . even) (4 :. 5 :. 6 :. Nil)
-- ExactlyOne [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
filtering ::
  Applicative k =>
  (a -> k Bool)
  -> List a
  -> k (List a)
filtering _ Nil = pure Nil
filtering f (x:.xs) = lift2 (\test as -> if test then x :. as else as) (f x) (filtering f xs)
  -- let next = filtering f xs in
  -- let allow = f x in 
  --   let append = lift2 (\b c -> if b then x :. c else c) allow next in
  --     append

  --(<*>) tab ta v = tab v (ta v)

    -- let zs = zip (map fbools xs) xs in
    --   let start = pure Nil in
    --     -- foldLeft (\acc (kbool, a) -> lift2 (++) acc (lift1 (\b -> if b then a :. Nil else Nil) kbool)) start zs
    --     foldLeft (\acc (kbool, a) -> lift2 (\g h -> g ++ (if h then a :. Nil else Nil)) acc kbool) start zs
-- filtering f (x:.xs) =       
--         foldLeft (\acc x -> lift2 (\f g -> if f then g ++ x :. Nil else g ) (f x) acc) (pure Nil) xs


  -- error "todo: Course.Applicative#filtering"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.pure
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

fail ::
  Applicative k =>
  Chars
  -> k a
fail =
  error . hlist

(>>) ::
  Applicative k =>
  k a
  -> k b
  -> k b
(>>) =
  (*>)
