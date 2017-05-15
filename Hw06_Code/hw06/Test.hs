module Test where

import Syntax
import Text.Parsec

import Test.QuickCheck

varName :: Gen String
varName = elements (map (:[]) ['a'..'z'])

instance Arbitrary Expr where
  arbitrary = sized $ expr
    where expr 0 = oneof [Var <$> varName,
                          (\x -> Lam x (Var x)) <$> varName,
                          (\x y -> Lam x (Lam y (Var x))) <$> varName <*> varName,
                          (\x y -> Lam x (Lam y (Var y))) <$> varName <*> varName]
          expr n = oneof [Var <$> varName,
                          Lam <$> varName <*> expr (n - 1),
                          App <$> expr (n `div` 2) <*> expr (n `div` 2)]

prop_prettyParse :: Expr -> Property
prop_prettyParse e = parseExpr' (show e) === Right e

prop_prettyParsePretty :: Expr -> Property
prop_prettyParsePretty e = show (parseExpr (show e)) === show e
