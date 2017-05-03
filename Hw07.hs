import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad.Except

data Type = TBool | TFun Type Type deriving (Eq, Show)

type VarName = String
data Expr =
    Var VarName
  | App Expr Expr
  | Lam VarName Expr
  | Bool Bool
  | If Expr Expr Expr deriving (Eq, Show)

type Context = Map VarName Type

data Error =
    UnboundVariable VarName
  | AppliedNonFunction Expr Type
  | Mismatch Expr Type Type -- failing expr, got, expected
    deriving Show

typeOf :: Context -> Expr -> Either Error Type
typeOf g (Var x) = 
  case Map.lookup x g of
    Just ty -> return ty
    Nothing -> Left $ UnboundVariable x
typeOf g (Bool b) = return TBool
typeOf g (Lam x t1 e) = do
  t2 <- typeOf (Map.insert x t1 g) e
  return $ TFun t1 t2
typeOf g e@(App e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case t1 of
    TFun t11 t12 | t2 == t11 -> return t12
    TFun t11 t12 -> throwError $ Mismatch e t2 t11
    _ -> throwError $ AppliedNonFunction e t1
typeOf g e@(If e1 e2 e3) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  t3 <- typeOf g e3
  case t1 of
    TBool | t2 == t3 -> return t2
    TBool -> throwError $ Mismatch e t3 t2
    _ -> throwError $ Mismatch e t1 Bool