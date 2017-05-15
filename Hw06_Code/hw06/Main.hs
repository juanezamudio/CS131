{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Syntax
import Text.Parsec
import Eval
import Check

import System.Environment
import System.Exit
import System.IO
import System.Posix.Files

import Data.List
import qualified Data.Set as Set

import System.Console.CmdArgs.Explicit

data Conf = Conf { source :: String,
                   checkScope :: Bool,
                   toNum :: Bool }

defaultConf = Conf { source = "-", checkScope = False, toNum = False }

arguments =
  mode
    "interp" [("file","-")]
    "lambda calculus interpreter"
    (flagArg (upd "file") "FILE (defaults to -, for stdin)")
    [flagNone ["check","c"] (flag "check") "Check scope",
     flagNone ["numeral","n"] (flag "numeral")
       "Convert final Church numeral to a number",
     flagHelpSimple (flag "help")]
  where upd msg x v = Right $ (msg,x):v
        flag name v = (name,""):v

main :: IO ()
main = do
  args <- processArgs arguments
  if ("help","") `elem` args
  then print $ helpText [] HelpFormatDefault arguments
  else do
    let conf = configure defaultConf args
    loadFile conf >>= parseText conf >>= check conf >>= run >>= number conf
    exitSuccess

configure :: Conf -> [(String,String)] -> Conf
configure = foldr update
  where update ("check",_) c = c{ checkScope = True }
        update ("numeral",_) c = c{ toNum = True }
        update ("file",f) c = c{ source = f }

parseText :: Conf -> String -> IO Program
parseText conf cts =
  case runParser program () (source conf) cts of
    Right e -> pure e
    Left err -> failWith $ "Parse error: " ++ show err

loadFile :: Conf -> IO String
loadFile (Conf {source="-"}) = getContents
loadFile (Conf {source=file}) = do
  exists <- fileExist file
  if exists
  then readFile file
  else failWith $ "No such file '" ++ file ++ "'"

check :: Conf -> Program -> IO Program
check (Conf {checkScope=False}) prog = pure prog
check (Conf {checkScope=True}) prog = do
  let fvs = fvExpr prog
  if Set.null fvs
    then pure prog
    else failWith $ "Unbound variables: " ++ intercalate ", " (Set.toList fvs)

run :: Program -> IO Expr
run e = do
  case eval e of
    Left err -> failWith $ "Error: " ++ show err
    Right e' -> return e'

number :: Conf -> Expr -> IO ()
number (Conf {toNum=False}) e = putStrLn $ show e
number (Conf {toNum=True})  e = 
  case eval (App (App e (Lam "x" (Succ (Var "x")))) Zero) of
    Left err -> failWith $ "Error: " ++ show err 
    Right v -> case fromNat v of
      Nothing -> failWith $ "Error: couldn't extract number from (alleged) Church numeral " ++ show e
      Just n -> putStrLn $ show n

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr $ msg
  exitFailure