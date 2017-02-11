module TplUntypedArithmetic where

data Term = ZTrue |
            ZFalse |
            ZIf Term Term Term |
            ZZero |
            ZSucc Term |
            ZPred Term |
            ZIsZero Term deriving (Show, Eq)

isValue :: Term -> Bool
isValue ZTrue  = True
isValue ZFalse = True
isValue x      = isNumerical x

isNumerical :: Term -> Bool
isNumerical (ZSucc x) = isNumerical x
isNumerical ZZero     = True
isNumerical _         = False

eval :: Term -> Term
eval (ZIf ZTrue ifTrue _)      = ifTrue
eval (ZIf ZFalse _ ifFalse)    = ifFalse
eval (ZIf cond ifTrue ifFalse) = ZIf (eval cond) ifTrue ifFalse
eval (ZSucc x)                 = ZSucc $ eval x
eval (ZPred ZZero)             = ZZero
eval (ZPred (ZSucc x))         | isNumerical x = x
eval (ZPred x)                 = ZPred $ eval x
eval (ZIsZero ZZero)           = ZTrue
eval (ZIsZero (ZSucc x))       | isNumerical x = ZFalse
eval (ZIsZero x)               = ZIsZero $ eval x
eval t                         = t

recEval :: Term -> Term
recEval t | t == (eval t) = t
recEval t = recEval $ eval t
