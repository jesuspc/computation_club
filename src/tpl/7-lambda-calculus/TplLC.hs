module TplLC (eval, Term (..)) where

import           Data.Maybe (maybe)

data Term = Var Int
          | Abs Term
          | App Term Term
          deriving (Eq, Show)

eval :: Term -> Term
eval t = maybe t eval (eval1 t)

eval1 :: Term -> Maybe Term
eval1 (App (Abs t1) t2)
  | isVal t2 = Just (termSubstTop t2 t1)
eval1 (App t1 t2)
  | isVal t1 = fmap (App t1) (eval1 t2)
  | otherwise = fmap (flip App t2) (eval1 t1)
eval1 _ = Nothing

isVal :: Term -> Bool
isVal (Abs _) = True
isVal _       = False

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

termShift :: Int -> Term -> Term
termShift d = walk 0
  where
    walk c t@(Var i)
      | i >= c = Var (i + d)
      | otherwise = t
    walk c (Abs t) = Abs $ walk (c + 1) t
    walk c (App t1 t2) = App (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
  where
    walk c t@(Var i)
      | i == j + c = termShift c s
      | otherwise = t
    walk c (Abs t1) = Abs $ walk (c + 1) t1
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
