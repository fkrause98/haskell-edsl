{-# LANGUAGE GADTs, KindSignatures, UnicodeSyntax #-}

module Shallow where

-- Implementación shallow utilizando Tagless-final
class Expr e where
  val :: Int -> e Int
  eq  :: e Int -> e Int -> e Bool
  lt  :: e Int -> e Int -> e Bool
  _not :: e Bool -> e Bool
  _and :: e Bool -> e Bool -> e Bool
  or  :: e Bool -> e Bool -> e Bool

data Eval e = Expr e

-- Definimos las distintas formas de evaluar
-- como instancias de Eval
instance Expr Eval where
  val x = Expr x
  eq (Expr x) (Expr y) = Expr (x == y)
  lt (Expr x) (Expr y) = Expr (x < y)
  _not (Expr x) = Expr ( Prelude.not x )
  _and (Expr x) (Expr y) = Expr ( x && y )
  or (Expr x) (Expr y) = Expr ( x || y )

instance Show t => Show ( Eval t ) where
  show (Expr e) = show e


-- Evaluación que hace string de una
-- forma que tipa.
data PrettyPrint a = PPrint String

-- Boilerplate para poder imprimir,
-- consta de simplemente devolver el
-- string que venimos acumulando.
instance Show ( PrettyPrint a ) where
  show (PPrint s) = s

-- Definimos cómo funciona un Expr con e = PrettyPPrint.
-- nos apoyamos en 'show' de Haskell para armar strings.
instance Expr PrettyPrint where
  val x = PPrint ( show x )
  eq (PPrint x) (PPrint y) = PPrint ( "(" ++ x ++ " = " ++ y ++ ")" )
  lt (PPrint x) (PPrint y) = PPrint ( "(" ++ x ++ " < " ++ y ++ ")" )
  _not (PPrint x) = PPrint ( "~" ++ x )
  _and (PPrint x) (PPrint y) = PPrint (x ++ " /" ++ "\\ " ++ y )
  or (PPrint x) (PPrint y) = PPrint( x ++ " \\" ++ "/ " ++ y )
