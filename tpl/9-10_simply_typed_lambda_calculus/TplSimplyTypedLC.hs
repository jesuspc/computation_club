module TplSimplyTypedLC where

import qualified Data.Map.Strict as Map

type Idx = Int
data Type = TypeBool | TypeArrow Type Type deriving (Show, Eq)
data Term = TmVar Idx
          | TmAbs Idx Type Term
          | TmApp Term Term
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          deriving (Show)

data Binding = Binding { bType :: Type }
type Context = Map.Map Idx Binding

addBind :: Idx -> Binding -> Context -> Context
addBind = Map.insert

getBind :: Idx -> Context -> Maybe Binding
getBind = Map.lookup

emptyCtx :: Context
emptyCtx = Map.empty

typeof :: Context -> Term -> Either String Type
typeof ctx TmTrue = Right TypeBool
typeof ctx TmFalse = Right TypeBool
typeof ctx (TmIf cond t1 t2) = typeof ctx cond >>= \t' ->
  let branchType1 = typeof ctx t1
      branchType2 = typeof ctx t2
      compare True  = branchType1
      compare False = Left "Arms of conditional have different types"
  in case t' of
    TypeBool -> (==) <$> branchType1 <*> branchType2 >>= compare
    _        -> Left "Guard of conditional was not Bool"
typeof ctx (TmVar i) =
  case getBind i ctx of
    Just binding -> Right (bType binding)
    Nothing      -> Left "Binding not found"
typeof ctx (TmAbs i ty t) = typeof (addBind i (Binding ty) ctx) t >>= \ty2 -> Right $ TypeArrow ty ty2
typeof ctx (TmApp t1 t2) = (,) <$> typeof ctx t1 <*> typeof ctx t2 >>= compare
  where
    compare (TypeArrow t11 t12, t13)
      | t11 == t13 = Right t12
      | otherwise = Left "Invalid application: unmatched types"
    compare _ = Left "Invalid application"
