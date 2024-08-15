{-# LANGUAGE GADTs, KindSignatures, UnicodeSyntax #-}

module Shallow where

-- Implementación shallow utilizando Tagless-final
class Expr e where
  val :: Int -> e Int
  eq  :: e Int -> e Int -> e Bool
  lt  :: e Int -> e Int -> e Bool
  not :: e Bool -> e Bool
  and :: e Bool -> e Bool -> e Bool
  or  :: e Bool -> e Bool -> e Bool

data Eval t where
  Eval :: t -> Eval t

-- Definimos las distintas formas de evaluar
-- como instancias de Eval
instance Expr Eval where
  val = Eval
  eq (Eval x) (Eval y) = Eval (x == y)
  lt (Eval x) (Eval y) = Eval (x < y)
  not (Eval x) = Eval ( Prelude.not x )
  and (Eval x) (Eval y) = Eval ( x && y )
  or (Eval x) (Eval y) = Eval ( x || y )

data Printable a = Print String

instance Show ( Printable a ) where
  show (Print s) = s

instance Expr Printable where
  val x = Print ( show x )
  eq (Print x) (Print y) = Print ( "(" ++ x ++ " = " ++ y ++ ")")
  -- lt (Eval x) (Eval y) = Eval (x < y)
  -- not (Eval x) = Eval ( Prelude.not x )
  -- and (Eval x) (Eval y) = Eval ( x && y )
  -- or (Eval x) (Eval y) = Eval ( x || y )

example :: Printable Int
example = val 1

example2 :: Printable Bool
example2 = eq (val 1) (val 1)
