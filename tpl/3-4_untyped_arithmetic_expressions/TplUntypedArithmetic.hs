module TplUntypedArithmetic where

data Term = ZTrue
          | ZFalse
          | ZIf Term Term Term
          | ZZero
          | ZSucc Term
          | ZPred Term
          | ZIsZero Term deriving (Show, Eq)

isValue :: Term -> Bool
isValue ZTrue  = True
isValue ZFalse = True
isValue x      = isNumerical x

isNumerical :: Term -> Bool
isNumerical (ZSucc x) = isNumerical x
isNumerical ZZero     = True
isNumerical _         = False

eval :: Term -> Maybe Term
eval (ZIf ZTrue ifTrue _)      = Just ifTrue
eval (ZIf ZFalse _ ifFalse)    = Just ifFalse
eval (ZIf cond ifTrue ifFalse) = fmap (\cond'-> ZIf cond' ifTrue ifFalse) (eval cond)
eval (ZSucc x)                 = fmap ZSucc (eval x)
eval (ZPred ZZero)             = Just ZZero
eval (ZPred (ZSucc x))         | isNumerical x = Just x
eval (ZPred x)                 = fmap ZPred (eval x)
eval (ZIsZero ZZero)           = Just ZTrue
eval (ZIsZero (ZSucc x))       | isNumerical x = Just ZFalse
eval (ZIsZero x)               = fmap ZIsZero (eval x)
eval t                         = Nothing

recEval :: Term -> Maybe Term
recEval t | isValue t = Just t
recEval t = eval t >>= recEval
