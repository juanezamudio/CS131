{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

module Hw06 where

import Prelude hiding (succ, pred, fst, snd)
import Control.Applicative
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt

type VarName = String
type Store = Map VarName Exp

update :: VarName -> Exp -> Store -> Store
update v n st = Map.insert v n st

data Exp = 
    Var VarName
  | Apply Exp Exp
  | Lambda VarName Exp
  | Let VarName Exp Exp
  deriving Eq

instance Show Exp where
  show = showExp

-- TODO: improve show
showExp :: Exp -> String
showExp (Var v) = v
showExp (Apply (Var v1) (Var v2)) = v1 ++ " " ++ v2
showExp (Apply (Var v) e2) = v ++ " (" ++ show e2 ++ ")"
showExp (Apply e1 (Var v)) = "(" ++ show e1 ++ ") " ++ v
showExp (Apply e1 e2) = show e1 ++ " (" ++ show e2 ++ ")"
showExp (Lambda v (Lambda v' e)) = "lambda " ++ v ++ " " ++ v' ++ ". " ++ show e
showExp (Lambda v e) = "lambda " ++ v ++ ". " ++ show e
showExp (Let v e1 e2) = "let " ++ v ++ " = " ++ show e1 ++ " in \n" ++ show e2

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }


instance Functor Parser where
  fmap f p = Parser $ \s -> (\(a,c) -> (f a, c)) <$> parse p s

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a,s)
  f <*> a = Parser $ \s ->
    case parse f s of
      Just (g,s') -> parse (fmap g a) s'
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  l <|> r = Parser $ \s -> parse l s <|> parse r s

-- ensures the next element parsed satisfy some requirement
ensure :: (a -> Bool) -> Parser a -> Parser a
ensure p parser = Parser $ \s ->
   case parse parser s of
     Nothing -> Nothing
     Just (a,s') -> if p a then Just (a,s') else Nothing

ensure2 :: (a -> Bool) -> (a -> Bool) -> Parser a -> Parser a
ensure2 p p' parser = Parser $ \s ->
   case parse parser s of
     Nothing -> Nothing
     Just (a,s') -> if p a && p' a then Just (a,s') else Nothing

-- parse one character
lookahead :: Parser (Maybe Char)
lookahead = Parser f
  where f [] = Just (Nothing,[])
        f (c:s) = Just (Just c,c:s)

-- parse a character only if it satisfy some requirement
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs) = if p x then Just (x,xs) else Nothing

-- parse the eof
eof :: Parser ()
eof = Parser $ \s -> if null s then Just ((),[]) else Nothing

-- parse white spaces
ws :: Parser ()
ws = pure () <* many (satisfy isSpace)

-- parse a certain character
char :: Char -> Parser Char
char c = ws *> satisfy (==c)

-- parse a certain string
str :: String -> Parser String
str s = ws *> loop s
  where loop [] = pure []
        loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs

-- parse the parenthesis
parens :: Parser a -> Parser a
parens p = (char '(' *> p) <* char ')'

keywords :: [String]
keywords = ["let","in","lambda"]

notKeyword :: String -> Bool
notKeyword = not . (`elem` keywords)

numOf' :: String -> Bool
numOf' s = (2::Int) > foldr (\x b -> if x == '\'' then (b+1) else b) 0 s

isAlphaNum' :: Char -> Bool
isAlphaNum' c = isAlphaNum c || c == '\''

-- parse a variable
var :: Parser String
var = ws *> ensure2 numOf' notKeyword ((:) <$> 
            satisfy isAlpha <*> many (satisfy isAlphaNum'))


chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p sep = foldl (\acc (op,v) -> op acc v) <$> 
                p <*> many ((\op v -> (op,v)) <$> sep <*> p)

lcSyntax :: Parser Exp
lcSyntax = (lcExp `chainl1` (pure Apply)) <* ws

lcExp :: Parser Exp
lcExp =     parens lcSyntax
        <|> Let <$> (str "let" *> var) <* char '=' <*> lcSyntax <* str "in" <*> lcSyntax
        <|> Lambda <$> (str "lambda" *> var) <*> lcLamda 
        <|> Var <$> var

lcLamda :: Parser Exp
lcLamda =    Lambda <$> var <*> lcLamda
         <|> char '.' *> lcSyntax

--lcProg1, lcProg2, lcProg3, lcProg4, lcProg2', lcProg2'' :: String
--lcProg1 = "s z"
--lcProg2 = "lambda x . x"
--lcProg2' = "lambda x . x x"
--lcProg2'' = "lambda x . x x x"
--lcProg3 = "lambda x . y"
--lcProg4 = "lambda x . x lambda y . y"

--zero = parseLC "lambda s z. z"
--succ = parseLC "lambda n. lambda s z. s (n s z)"
--two = parseLC "let zero = lambda s z. z in let succ = lambda n. lambda s z. s (n s z) in succ (succ zero)"

--pred = parseLC "\
--    \let zero = lambda s z. z in \
--    \let succ = lambda n. lambda s z. s (n s z) in \
--    \let pair = lambda a b. lambda c. c a b in \
--    \let fst = lambda p. p (lambda f t. f) in \
--    \let snd = lambda p. p (lambda f t. t) in \
--    \lambda n. snd (n (lambda p. pair (succ (fst p)) (fst p)) (pair zero zero))"

tryParse :: Parser Exp -> String -> Either String Exp
tryParse parser s = 
    case parse parser s of
      Nothing -> Left "Cannot parse the expression"
      Just (x, "") -> Right x
      Just (_, s') -> Left $ "Expected EOF, got:" ++ s'

parseLC :: String -> Either String Exp
parseLC = tryParse lcSyntax

interp :: Exp -> Exp
interp (Var v) = Var v
interp (Apply e1 e2) = 
  case interp e1 of
    Lambda v e -> case interp e2 of 
                    Var e2' -> interp (bSub v (Var e2') e)
                    e2' -> interp (bSub v e2' e)
    _ -> undefined
interp (Let v e1 e2) = interp (bSub v e1 e2)
interp (Lambda v e) = Lambda v e

bSub :: VarName -> Exp -> Exp -> Exp
bSub v e (Var v') = if v' == v then e else Var v'
bSub v e (Apply e1 e2) = Apply (bSub v e e1) (bSub v e e2)
bSub v e (Let v' e1 e2) = if v' == v then Let v' e1 e2
                          else Let v' (bSub v e e1) (bSub v e e2)
bSub v e (Lambda v' e') = if v' == v then Lambda v' e'
                          else Lambda v' (bSub v e e')



check :: [Flag] -> Exp -> Either String (Set VarName)
check as x = if Check `elem` as then check' Set.empty x else Right Set.empty

check' :: Set VarName -> Exp -> Either String (Set VarName)
check' s (Var v) = if v `Set.notMember` s then Left v else Right s
check' s (Apply e1 e2) = case check' s e1 of
                            Left uv -> Left uv
                            Right _ -> check' s e2
check' s (Lambda v e) = case check' (v `Set.insert` s) e of
                            Left uv -> Left uv
                            Right _ -> Right s
check' s (Let v e1 e2) = case check' s e1 of
                            Left uv -> Left uv
                            Right _ -> check' (v `Set.insert` s) e2


toNumeral :: Exp -> Maybe Int
toNumeral (Lambda _ (Lambda z (Var z'))) | z == z' = Just 0
toNumeral (Lambda z (Var z')) | z == z' = Just 1
toNumeral (Lambda s (Lambda z (Apply (Var s') (Apply (Apply e (Var s'')) (Var z'))))) 
  = if s == s' && s == s'' && z == z' then (+1) <$> toNumeral e
    else Nothing 
toNumeral (Lambda s (Lambda z (Apply (Var s') e))) | s == s' = 
    (+1) <$> toNumeral (Lambda s (Lambda z e))
toNumeral _ = Nothing 



data Flag = Check                -- -c
          | Numeral              -- -n
          | Help                 -- --help
           deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags =
   [Option ['c'] []       (NoArg Check)
        "Check if all variables are bound."
   ,Option ['n'] []       (NoArg Numeral)
        "Convert lambda expression to Church-Numeral."
   ,Option []    ["help"] (NoArg Help)
        "Print this help message"
   ]

parseArgs :: [String] -> IO ([Flag], [[Char]])
parseArgs argv = case getOpt Permute flags argv of
      (args,fs,[]) -> do
          let files = if null fs then ["-"] else fs
          if Help `elem` args
              then do hPutStrLn stderr (usageInfo header flags)
                      exitWith ExitSuccess
              else return (args, files)

      (_,_,errs)      -> do
          hPutStrLn stderr (concat errs ++ usageInfo header flags)
          exitWith (ExitFailure 1)

      where header = "Usage: interp [-cn] [file]"

main :: IO ()
main = do
    (as, fs) <- getArgs >>= parseArgs
    content <- if head fs == "-" then getContents else readFile (head fs)
    case tryParse lcSyntax content of
      Left e -> die e
      Right x -> 
        case check as x of 
          Left vs -> die $ "Unbound variable: " ++ vs
          Right _ -> 
            if Numeral `elem` as 
            then case toNumeral (interp x) of
                  Just n -> print n
                  Nothing -> die "The expression cannot be converted to Church-Numeral"
            else case interp x of
                  Var v -> die $ "Unbound variable: " ++ v
                  s -> print s
