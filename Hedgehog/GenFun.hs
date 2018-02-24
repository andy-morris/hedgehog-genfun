-- |
-- Module:      Hedgehog.GenFun
-- Description: Function generation for Hedgehog
-- Copyright:   © 2018 Andrew Morris
-- Licence:     BSD3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: Numerous language extensions
--
-- Function generation for @hedgehog@.
--
-- Functions are represented as maps from certain arguments to results, along
-- with a default result. This means that all but only a few values in the
-- domain will have the same result.
--
-- === Example
--
-- @
-- intFun :: Gen (Fun '[Int, Int] Int)
-- intFun = genFun $ int ':->' int ':->.' int
--   where int = Gen.int (Range.linear 1 1000)
--
-- prop_foo :: Property
-- prop_foo = property $ do
--   f           <- forAll intFun
--   (x, (y, _)) <- forAll $ argsFor f intFun
--   eval $ appFun f x y
-- @
{-# LANGUAGE
    ConstraintKinds, DataKinds, GADTs, KindSignatures, PatternSynonyms,
    PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances,
    TypeInType
  #-}
module Hedgehog.GenFun
  (Fun (..), FunType, appFun, All,
   FunGens (.., (:->.)), genFun,
   Pairs, appFunPairs, argsFor, pattern Arg2, pattern Arg3)
where

import Data.Kind (Type, Constraint)

import Text.Show (showListWith)

import Hedgehog
import qualified Hedgehog.Gen as Gen


-- | Constructs a function type from a list of arguments and a result.
--
-- @FunType [a1, ..., an] r ~ a1 -> ... -> an -> r@
type family FunType (as :: [Type]) (r :: Type) :: Type where
  FunType '[]       r = r
  FunType (a ': as) r = a -> FunType as r

-- | A function with each argument represented as an association list plus
-- a default value.
data Fun (as :: [Type]) (r :: Type) where
  -- | A return value
  FRet :: r                           -> Fun '[]       r
  -- | Some cases, and a default
  FArg :: [(a, Fun as r)] -> Fun as r -> Fun (a ': as) r

-- | @All p as@ is satisfied if there is an instance of @p@
-- for each @a@ in @as@.
type family All (p :: k -> Constraint) (as :: [k]) :: Constraint where
  All _ '[]       = ()
  All p (a ': as) = (p a, All p as)


-- | Apply a single argument of a 'Fun' by looking it up in the list, or using
-- the default value otherwise.
app1 :: Eq a => [(a, r)] -> r -> a -> r
app1 binds def x =
  case lookup x binds of
    Just y  -> y
    Nothing -> def

-- | Apply a 'Fun' to its arguments.
appFun :: All Eq as => Fun as r -> FunType as r
appFun (FRet r)         = r
appFun (FArg binds def) = appFun . app1 binds def


-- | Printed as nested maps, each of which is a list of pairs written
-- @in :~> out@. The default value is written @Else :~> out@.
--
-- This strange representation is chosen because it plays nicely with
-- @pretty-show@.
instance (All Show as, Show r) => Show (Fun as r) where
  showsPrec _ (FRet r)         = shows r
  showsPrec _ (FArg binds def) =
      showListWith id $ map showBind binds ++ [showDef def]
    where
      showBind (a, r) =
        showsPrec 1 a . showString " :~> " . showsPrec 1 r
      showDef r =
        showString "Else :~> " . showsPrec 1 r


-- | The input generators needed to generate a function: one for each argument
-- and one for the return value.
data FunGens (as :: [Type]) (r :: Type) where
  -- | A generator for the return value
  Ret   :: Gen r                 -> FunGens '[] r
  -- | A generator for the first argument, and then the others.
  (:->) :: Gen a -> FunGens as r -> FunGens (a ': as) r
infixr 0 :->

-- | @a :->. r@ is an abbreviation for @a ':->' 'Ret' r@.
pattern (:->.) :: Gen a -> Gen r -> FunGens '[a] r
pattern a :->. r = a :-> Ret r
infixr 0 :->.

{-# COMPLETE (:->), (:->.) #-}


-- | Generate a 'Fun' from some generators for the arguments and return value.
-- The 'Range' argument controls the number of non-default cases for each
-- argument.
genFun :: Range Int -> FunGens as r -> Gen (Fun as r)
genFun _  (Ret r)   = FRet <$> r
genFun sz (a :-> r) = FArg <$> binds <*> def where
  binds = Gen.list sz $ (,) <$> Gen.small a <*> def
  def   = Gen.small $ genFun sz r


-- | Makes a list of types into nested tuples.
--
-- @Pairs '[Int, String, Float, [Int]] ~ (Int, (String, (Float, ([Int], ()))))@
type family Pairs (xs :: [Type]) :: Type where
  Pairs '[]       = ()
  Pairs (x ': xs) = (x, Pairs xs)


-- | Same as 'appFun', but applying the function to nested pairs instead of to
-- separate arguments.
--
-- @appFunPairs f (x1, (..., (xn, ()))) === appFun f x1 ... xn@
appFunPairs :: All Eq as => Fun as r -> Pairs as -> r
appFunPairs (FRet r)         ()      = r
appFunPairs (FArg binds def) (x, xs) = appFunPairs (app1 binds def x) xs

-- | Generate some interesting arguments for a 'Fun'. Each value has an
-- increased chance of being one of the non-default cases for that argument, if
-- it hash any.
argsFor :: Fun as r -> FunGens as r -> Gen (Pairs as)
argsFor (FRet _)         _         = pure ()
argsFor (FArg binds def) (a :-> r) = Gen.frequency
    [(4, Gen.choice $ map chooseBind binds),
     (1, (,) <$> a <*> argsFor def r)]
  where chooseBind (a', f) = (,) a' <$> argsFor f r

-- | Abbreviation for two arguments.
pattern Arg2 :: a -> b -> Pairs '[a, b]
pattern Arg2 a b = (a, (b, ()))
{-# COMPLETE Arg2 #-}

-- | Abbreviation for three arguments.
pattern Arg3 :: a -> b -> c -> Pairs '[a, b, c]
pattern Arg3 a b c = (a, (b, (c, ())))
{-# COMPLETE Arg3 #-}
