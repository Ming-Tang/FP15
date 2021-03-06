{-# LANGUAGE Trustworthy, GADTs #-}
module FP15.Evaluator.Contract where
import Control.Monad(guard, liftM2, liftM3, liftM4, liftM5)
import Data.Maybe(isJust, catMaybes)
import Data.These(These(..))
import FP15.Value
import FP15.Evaluator.Types

-- * Common Contracts

listAnyC :: Contract [FPValue]
any2C :: Contract (FPValue, FPValue)
any3C :: Contract (FPValue, FPValue, FPValue)
any4C :: Contract (FPValue, FPValue, FPValue, FPValue)
any5C :: Contract (FPValue, FPValue, FPValue, FPValue, FPValue)

listAnyC = ListC AnyC
any2C = Args2C AnyC AnyC
any3C = Args3C AnyC AnyC AnyC
any4C = Args4C AnyC AnyC AnyC AnyC
any5C = Args5C AnyC AnyC AnyC AnyC AnyC

-- * Validation

-- | The 'validate' function validates a 'Value' against a 'Contract'. If the
-- value is validated by the contract, then 'Just' of the extracted value will
-- be returned. Otherwise, if the validation fails, then 'Nothing' is returned.
validate :: Contract a -> FPValue -> Maybe a
-- | The 'validateList' function validates a list of values against a contract.
-- If /all/ items in the list have been validated, then 'Just' of the extracted
-- values for each element will be returned. Otherwise, 'Nothing' is returned.
validateList :: Contract a -> [FPValue] -> Maybe [a]

validateList c xs = do
  let valids = map (validate c) xs
  guard (all isJust valids)
  return $ catMaybes valids

validate AnyC x = Just x
validate RealWorldC rw@(Extended (RealWorld RW)) = Just RW
validate RealWorldC _ = Nothing
validate ValueC (Extended _) = Nothing
validate ValueC v = fromFPValue v

validate BoolC (Bool b) = Just b
validate SymbolC (Symbol s) = Just (Sym s)
validate StringC (String s) = Just (Str s)

validate CharC (Char c) = Just c
validate IntC (Int i) = Just i
validate RealC (Real r) = Just r
validate NumberC (Int i) = Just (IntN i)
validate NumberC (Real r) = Just (RealN r)

validate (ListC c) (List xs) = validateList c xs
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

validate (AndC a b) x = liftM2 (ub Both (,)) (validate a x) (validate b x)
  where ub = (.) (.) (.)
validate (OrC a b) x = fromMaybes (validate a x) (validate b x)
  where fromMaybes Nothing Nothing = Nothing
        fromMaybes (Just p) Nothing = Just (This p)
        fromMaybes Nothing (Just q) = Just (That q)
        fromMaybes (Just p) (Just q) = Just (These p q)
validate (EitherC a b) x = validate a x `combine` validate b x
  where combine Nothing Nothing = Nothing
        combine (Just p) Nothing = Just (Left p)
        combine Nothing (Just q) = Just (Right q)
        combine (Just p) (Just q) = Just (Left p)

-- I list out cases explicitly so if I add a new case, I get a compiler warning.

validate BoolC _ = Nothing
validate SymbolC _ = Nothing
validate StringC _ = Nothing

validate CharC _ = Nothing
validate IntC _ = Nothing
validate RealC _ = Nothing
validate NumberC _ = Nothing

validate (ListC _) _ = Nothing

validate EmptyC _ = Nothing
validate (ConsC {}) _ = Nothing

validate (Args2C {}) _ = Nothing
validate (Args3C {}) _ = Nothing
validate (Args4C {}) _ = Nothing
validate (Args5C {}) _ = Nothing

