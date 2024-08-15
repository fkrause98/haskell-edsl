{-# LANGUAGE GADTs, KindSignatures, UnicodeSyntax #-}
module Main where

import Shallow
import Prelude hiding (and)

main :: IO ()
main = do
  putStrLn $ (show example)
  putStrLn $ (show exampleEq)
  putStrLn $ (show exampleLt)
  putStrLn $ (show exampleNot)


example :: Printable Int
example = val 1

exampleEq :: Printable Bool
exampleEq = eq (val 1) (val 1)

exampleLt :: Printable Bool
exampleLt = lt (val 1) (val 1)

exampleNot :: Printable Bool
exampleNot = _not (lt (val 1) (val 1))

-- exampleAnd :: Printable Bool
-- exampleAnd = _and (lt (val 1) (val 1))
