{-# LANGUAGE GADTs, ExistentialQuantification, ImpredicativeTypes #-}
module FP15.Evaluator.Contract where
import Control.Monad(guard, liftM2, liftM3, liftM4, liftM5)
import Data.Maybe(isJust, catMaybes)
import Data.These(These(..))
import FP15.Value(Value(..))
import FP15.Evaluator.Types

listAnyC = ListC AnyC
any2C = Args2C AnyC AnyC
any3C = Args3C AnyC AnyC AnyC
any4C = Args4C AnyC AnyC AnyC AnyC
any5C = Args5C AnyC AnyC AnyC AnyC AnyC

validate :: Contract a -> Value -> Maybe a
validateList :: Contract a -> [Value] -> Maybe [a]

validateList c xs =
  do
    let valids = map (validate c) xs
    guard (all isJust valids)
    return $ catMaybes valids

validate AnyC x = Just x

validate BoolC (Bool b) = Just b
validate SymbolC (Symbol s) = Just (Sym s)
validate StringC (String s) = Just (Str s)

validate CharC (Char c) = Just c
validate IntC (Int i) = Just i
validate RealC (Real r) = Just r
validate NumberC (Int i) = Just (IntN i)
validate NumberC (Real r) = Just (RealN r)

validate (ListC c) (List xs) = validateList c xs
validate (NonEmptyListC c) (List []) = Nothing
validate (NonEmptyListC c) (List xs) = validateList c xs
validate EmptyC (List []) = Just ()

validate (ConsC c cs) (List (x:xs)) =
  liftM2 (curry Cons) (validate c x) (validate cs (List xs))

validate (Args2C c1 c2) (List [a, b]) =
  liftM2 (,) (validate c1 a) (validate c2 b)
validate (Args3C c1 c2 c3) (List [a, b, c]) =
  liftM3 (,,) (validate c1 a) (validate c2 b) (validate c3 c)
validate (Args4C c1 c2 c3 c4) (List [a, b, c, d]) =
  liftM4 (,,,) (validate c1 a) (validate c2 b) (validate c3 c) (validate c4 d)
validate (Args5C c1 c2 c3 c4 c5) (List [a, b, c, d, e]) =
  liftM5 (,,,,) (validate c1 a) (validate c2 b) (validate c3 c)
                (validate c4 d) (validate c5 e)

validate (NotC a) x =
  case validate a x of
    Nothing -> Just (Never x)
    Just y -> Nothing
validate (AndC a b) x = liftM2 (ub Both (,)) (validate a x) (validate b x)
  where ub = (.) (.) (.)
validate (OrC a b) x = fromMaybes (validate a x) (validate b x)
  where fromMaybes Nothing Nothing = Nothing
        fromMaybes (Just p) Nothing = Just (This p)
        fromMaybes Nothing (Just q) = Just (That q)
        fromMaybes (Just p) (Just q) = Just (These p q)
validate (XorC a b) x = xorMaybes (validate a x) (validate b x)
  where xorMaybes Nothing Nothing = Nothing
        xorMaybes (Just p) Nothing = Just (Left p)
        xorMaybes Nothing (Just q) = Just (Right q)
        xorMaybes (Just p) (Just q) = Nothing

-- I list out cases explicitly so if I add a new case, I get a compiler warning

validate BoolC _ = Nothing
validate SymbolC _ = Nothing
validate StringC _ = Nothing

validate CharC _ = Nothing
validate IntC _ = Nothing
validate RealC _ = Nothing
validate NumberC _ = Nothing

validate (ListC _) _ = Nothing
validate (NonEmptyListC _) _ = Nothing

validate EmptyC _ = Nothing
validate (ConsC {}) _ = Nothing

validate (Args2C {}) _ = Nothing
validate (Args3C {}) _ = Nothing
validate (Args4C {}) _ = Nothing
validate (Args5C {}) _ = Nothing

