{-# LANGUAGE FlexibleContexts #-}

module Syntax where

import Text.Parsec

import Control.Monad.Identity
import Data.Either

import Text.Printf
import Data.List

type VarName = String

data Expr =
    Var VarName
  | Lam VarName Expr
  | App Expr Expr
  | Zero
  | Succ Expr
  deriving Eq


instance Show Expr where
  show = showExpr 0
    where
      showExpr 0 (App (Lam x e2) e1) = "let " ++ x ++ " = " ++ showExpr 0 e1 ++ " in " ++ showExpr 0 e2
      showExpr 0 e@(App _ _) =
        let es = collectApps e in
        intercalate " " $ map (showExpr 1) es
      showExpr 0 (Succ e) = printf "SUCC %s" (showExpr 1 e)
      showExpr 0 (Lam x e) =
        let (e', vars) = collectVars e in
        printf "lambda %s. %s" (intercalate " " (x:vars)) (showExpr 0 e')
      showExpr 0 e = showExpr 1 e
      showExpr 1 (Var x) = x
      showExpr 1 Zero = "ZERO"
      showExpr 1 e = showExpr 2 e
      showExpr 2 e = printf "(%s)" (showExpr 0 e)
      showExpr _ e = error "bad precedence index for showExpr"
      collectApps (App e1 e2) = collectApps e1 ++ [e2]
      collectApps e = [e]
      collectVars (Lam x e) = (x:) <$> collectVars e
      collectVars e = (e,[])

type Program = Expr

-- parser, using Parsec

parseProgram :: String -> Either Text.Parsec.ParseError Expr
parseProgram = runParser program () "stdin"

parseExpr :: String -> Expr
parseExpr s =
  case parseExpr' s of
    Right e -> e
    Left err -> error $ show err

parseExpr' :: String -> Either Text.Parsec.ParseError Expr
parseExpr' = runParser expr () "stdin"

omega = parseExpr "(lambda x. x x) (lambda x. x x)"

keywords = ["let","in","lambda"]
isKeyword x = x `elem` keywords

program :: Stream s m Char => ParsecT s () m Program
program = expr <* ws <* eof

expr, letin, atom, lam, var :: Stream s m Char => ParsecT s () m Expr
expr = try letin <|> foldl1 App <$> (atom `sepEndBy1` ws)
letin = (\x e1 e2 -> (App (Lam x e2) e1)) <$> (kw "let" *> identifier) <*> (symbol "=" *> expr) <*> (kw "in" *> expr)
atom = try lam <|> try var <|> parens expr
lam = do
  ids <- kw "lambda" *> space *> many1 (try identifier)
  body <- symbol "." *> expr
  pure $ foldr Lam body ids
var = Var <$> identifier
parens :: Stream s m Char => ParsecT s () m a -> ParsecT s () m a
parens = between (symbol "(") (symbol ")")

identifier :: Stream s m Char => ParsecT s () m String
identifier = do
  ws
  x <- (:) <$> letter <*> many (alphaNum <|> char '\'')
  if isKeyword x
  then unexpected $ "keyword in place of variable (" ++ x ++ ")"
  else pure x

kw :: Stream s m Char => String -> ParsecT s () m ()
kw s = symbol s *> notFollowedBy alphaNum

symbol :: Stream s m Char => String -> ParsecT s () m String
symbol s = ws *> string s

ws, comment :: Stream s m Char => ParsecT s () m ()
ws = many (try comment <|> space *> pure ()) *> pure ()
comment = string "--" *> manyTill anyChar eol *> pure ()
  where eol = (try endOfLine *> pure ()) <|> eof