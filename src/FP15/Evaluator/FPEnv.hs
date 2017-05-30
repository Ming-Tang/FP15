{-# LANGUAGE DeriveGeneric #-}
module FP15.Evaluator.FPEnv where
import GHC.Generics(Generic)
import Control.DeepSeq

newtype FPEnv v = FPEnv { getFPEnv :: [v] } deriving (Generic)

instance NFData (FPEnv v) where rnf x = seq x ()

initial :: FPEnv v
initial = FPEnv []

getCtx :: Int -> FPEnv v -> Maybe v
getCtx i (FPEnv vs) = safeIndex i vs where
  safeIndex n (x:xs) | n <= 0 = Just x
                     | otherwise = safeIndex (n - 1) xs
  safeIndex _ [] = Nothing

push :: v -> FPEnv v -> FPEnv v
push x (FPEnv xs) = FPEnv (x:xs)

pop :: FPEnv v -> FPEnv v
pop (FPEnv (_:xs)) = FPEnv xs
pop (FPEnv []) = FPEnv []
