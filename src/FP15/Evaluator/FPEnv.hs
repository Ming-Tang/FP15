{-# LANGUAGE DeriveGeneric #-}
module FP15.Evaluator.FPEnv where
import GHC.Generics(Generic)
import Control.DeepSeq
import FP15.Evaluator.FPValue

newtype FPEnv = FPEnv { getFPEnv :: [FPValue] } deriving (Generic)

instance NFData FPEnv where

initial :: FPEnv
initial = FPEnv []

get :: Int -> FPEnv -> Maybe FPValue
get i (FPEnv vs) = safeIndex i vs where
  safeIndex n (x:xs) | n <= 0 = Just x
                     | otherwise = safeIndex (n - 1) xs
  safeIndex _ [] = Nothing

push :: FPValue -> FPEnv -> FPEnv
push x (FPEnv xs) = FPEnv (x:xs)

pop :: FPEnv -> FPEnv
pop (FPEnv (_:xs)) = FPEnv xs
pop (FPEnv []) = FPEnv []
