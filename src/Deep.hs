{-# LANGUAGE GADTs, KindSignatures, UnicodeSyntax, DataKinds, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Deep where

-- Implementación deep embedding con GADTs
data Expr :: (* -> *) where
  Val :: Int -> Expr Int
  Eq  :: Expr Int -> Expr Int -> Expr Bool
  Lt  :: Expr Int -> Expr Int -> Expr Bool
  Not :: Expr Bool -> Expr Bool
  And :: Expr Bool -> Expr Bool -> Expr Bool
  Or  :: Expr Bool -> Expr Bool -> Expr Bool

-- Type class para definir
-- observadores, nos servimos del t
-- para tener type safety.
class ExprReduce t where
  eval :: Expr t -> t
  showExpr :: Expr t -> String

-- Instance for evaluating and showing Int expressions
instance ExprReduce Int where
  eval (Val x) = x
  showExpr (Val x) = show x

instance ExprReduce Bool where
  -- Usamos eval recursivamente
  eval (Not x) = not(eval x)
  eval binaryExpr = case binaryExpr of
    Eq x y -> eval x == eval y
    Lt x y -> eval x <= eval y
    And x y -> eval x && eval y
    Or x y -> eval x || eval y

  showExpr (Not x) =  "~" ++ showExpr x
  showExpr  bExpr = showBinaryExpr bExpr

showBinaryExpr :: Expr Bool -> String
showBinaryExpr (x `Eq` y) = "(" ++ showExpr x ++ " " ++ "=" ++ " " ++ showExpr y ++ ")"
showBinaryExpr (x `Lt` y) = "(" ++ showExpr x ++ " " ++ "<" ++ " " ++ showExpr y ++ ")"
showBinaryExpr (x `And` y) = showExpr x ++ " " ++ " /" ++ "\\ " ++ " " ++ showExpr y
showBinaryExpr (x `Or` y) = showExpr x ++ " " ++ " \\" ++ "/ " ++ " " ++ showExpr y
