{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S
import Data.String

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck(Fun)
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State (s -> (a, s))

runState ::
  State s a
  -> s
  -> (a, s)
runState (State f) b = f b

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
-- +++ OK, passed 100 tests.
exec :: State s a -> s -> s
exec st = snd . runState st
-- exec (State sf) s = snd (sf s)

  -- error "todo: Course.State#exec"

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
-- +++ OK, passed 100 tests.
eval ::
  State s a
  -> s
  -> a
-- eval (State sf) s = fst (sf s)
eval st = fst . runState st
  -- error "todo: Course.State#eval"

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State (\a -> (a, a))
  -- error "todo: Course.State#get"

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s = State (\_ -> ((), s))
  -- error "todo: Course.State#put"

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  -- (<$>) fab (State sas) = State (\v -> (fab (fst (sas v)), snd (sas v)))
  (<$>) f sa =  State (\s -> let (a, s') = runState sa s in (f a, s'))
  -- (<$>) fab ssa = State (\s -> (fab (eval ssa s), exec ssa s))
    -- error "todo: Course.State#(<$>)"

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> runState (State (\s -> ((+3), s ++ ("apple":.Nil))) <*> State (\s -> (7, s ++ ("banana":.Nil)))) Nil
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a = State (\s -> (a, s))
    -- error "todo: Course.State pure#instance (State s)"
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  -- (<*>) sab ssa = State (\s -> 
  --   let (f, s2) = runState sab s in 
  --     (f (eval ssa s2), exec ssa s2))
  (<*>) ssab ssa = State (\s ->
    case runState ssab s of
      (fab, s') -> case runState ssa s' of
        (a, s'') -> (fab a, s'')
    )
    -- error "todo: Course.State (<*>)#instance (State s)"

-- | Implement the `Monad` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
--
-- >>> runState ((\a -> State (\s -> (a + s, 10 + s))) =<< State (\s -> (s * 2, 4 + s))) 2
-- (10,16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  (=<<) fasb ssa = State (\s ->
    case runState ssa s of
      (a, s') -> runState (fasb a) s')

    -- error "todo: Course.State (=<<)#instance (State s)"

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = pure Empty
-- findM f (h :. t) = join $ lift1 (\a -> if a then pure $ Full h else findM f t) $ f h
-- findM f xs = foldRight (\a b -> let fa = f a in (\g -> if g then pure $ Full a else b) =<< fa) (pure Empty) xs
findM f (h :. t) =  (\g -> if g then pure $ Full h else findM f t ) =<< f h
  -- error "todo: Course.State#findM"

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- >>> firstRepeat $ 1 :. 2 :. 0 :. 9 :. 2 :. 1 :. Nil
-- Full 2
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- +++ OK, passed 100 tests.
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
-- +++ OK, passed 100 tests.
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat Nil = Empty
firstRepeat xs = let p = \a ->
                        -- (\s -> const (pure (S.member a s)) =<< put (S.insert a s)) =<< get
                        -- State (\s -> (S.member a s, S.insert a s))
                        (\s -> S.member a s <$ put (S.insert a s)) =<< get -- Put returns ((), Set), and the <$ modifies the () to be the bool
                        in
  eval (findM p xs) S.empty

  -- error "todo: Course.State#firstRepeat"

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
-- +++ OK, passed 100 tests.
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
-- +++ OK, passed 100 tests.
distinct ::
  Ord a =>
  List a
  -> List a
distinct xs = let p = \a ->
                        -- (\s -> const (pure (S.notMember a s)) =<< put (S.insert a s))  =<< get 
                        (\s -> S.notMember a s <$ put (S.insert a s)) =<< get
                       in
    eval (filtering p xs) S.empty
  -- error "todo: Course.State#distinct"

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy a = contains 1 . firstRepeat . produce nextHappyNumber $ P.fromIntegral a

nextHappyNumber :: Int -> Int
-- nextHappyNumber x = sum $ map (\b -> let m = digitToInt b in (m * m) ) (show' x)
nextHappyNumber x = sum (join (*) . digitToInt <$> show' x)

