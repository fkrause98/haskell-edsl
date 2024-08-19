{-# LANGUAGE GADTs, KindSignatures, UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Shallow

main :: IO ()
main = do
  print "Printing examples built with Shallow"
  putStrLn $ renderShallowExample example example
  putStrLn $ renderShallowExample exampleEq exampleEq
  putStrLn $ renderShallowExample exampleLt exampleLt
  putStrLn $ renderShallowExample exampleNot exampleNot
  putStrLn $ renderShallowExample exampleAnd exampleAnd


renderShallowExample :: (Show e) => PrettyPrint e  -> Eval e -> String
-- renderShallowExample pretty evaled = pure ((show pretty :: PrettyPrint e) ++ " ≣ " ++ (show evaled))
renderShallowExample pretty evaled = concat [show pretty, " ≣ ", show evaled]

example :: Expr e => e Int
example = val 1

exampleEq ::  Expr e => e Bool
exampleEq = eq (val 1) (val 1)

exampleLt ::  Expr e => e Bool
exampleLt = lt (val 1) (val 1)

exampleNot ::  Expr e => e Bool
exampleNot = _not (lt (val 1) (val 1))

exampleAnd ::  Expr e => e Bool
exampleAnd = _and (lt (val 2) (val 1)) (lt (val 2) (val 1))
