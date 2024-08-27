-- Nombre: Francisco Krause Arnim
-- Apellido: Krause Arnim
-- Universidad: Universidad de Buenos Aires
-- Libreta Universitaria: 99/19
-- Email: fkrausear@gmail.com
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Deep
import Parser (parseProp)
import Shallow hiding (Expr)
import qualified Shallow as Sha (Expr)

-- Funcion main para solamente imprimir
-- los datos y funciones definidos en el proyecto,
-- a modo de prueba.

main :: IO ()
main = do
  putStrLn "Imprimiendo ejemplos construidos y evaluados mediante Shallow!"
  putStrLn $ renderShallowExample example example
  putStrLn $ renderShallowExample exampleEq exampleEq
  putStrLn $ renderShallowExample exampleLt exampleLt
  putStrLn $ renderShallowExample exampleNot exampleNot
  putStrLn $ renderShallowExample exampleAnd exampleAnd
  putStrLn $ "Imprimiendo ejemplos construidos y evaluados mediante Deep!"
  putStrLn $ renderDeepExample exampleDeep
  putStrLn $ renderDeepExample exampleLtDeep
  putStrLn $ renderDeepExample exampleEqDeep
  putStrLn $ renderDeepExample exampleNotDeep
  putStrLn $ renderDeepExample exampleAndDeep
  putStrLn "Imprimiendo resultados de parseo!"
  let exampleParse1 = "1"
  let exampleParse2 = "~(3<4)"
  let exampleParse3 = "(1 /\\ 2) \\/ ∼(3 < 4)"
  putStrLn (exampleParse1 ++ "  ≣   " ++ show (parseProp exampleParse1))
  putStrLn (exampleParse2 ++ "--->" ++ show (parseProp exampleParse2))
  putStrLn (exampleParse3 ++ "--->" ++ show (parseProp exampleParse3))

-- print $ parseProp exampleParse2
-- print $ parseProp exampleParse2

-- Dado una expresion armada mediante la técnica Shallow,
-- la representamos como String y como su valor de verdad.
renderShallowExample :: (Show e) => PrettyPrint e -> Eval e -> String
renderShallowExample pretty evaled = concat [show pretty, " ≣ ", show evaled]

-- Idem arriba, pero con técnica Embed.
renderDeepExample expr = concat [showExpr expr, " ≣ ", show $ eval expr]

example :: (Sha.Expr e) => e Int
example = val 1

exampleEq :: (Sha.Expr e) => e Bool
exampleEq = eq (val 1) (val 1)

exampleLt :: (Sha.Expr e) => e Bool
exampleLt = lt (val 1) (val 1)

exampleNot :: (Sha.Expr e) => e Bool
exampleNot = _not (lt (val 1) (val 1))

exampleAnd :: (Sha.Expr e) => e Bool
exampleAnd = _and (lt (val 2) (val 1)) (lt (val 2) (val 1))

exampleDeep = Val 1

exampleLtDeep = Lt (Val 5) (Val 1)

exampleEqDeep = Eq (Val 2) (Val 2)

exampleNotDeep = Not (Eq (Val 1) (Val 1))

exampleAndDeep = And (Eq (Val 1) (Val 1)) (Eq (Val 13) (Val 13))
