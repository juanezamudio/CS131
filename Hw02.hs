import Prelude

intercalate :: [a] -> [[a]] -> [a]
intercalate x lst = foldr (\y acc -> y ++ x ++ acc) [] lst

intercalate' :: [a] -> [[a]] -> [a]
intercalate' a (x:xs) = x ++ (foldr(\y acc -> a ++ y ++ acc) [] xs)

all' :: (a -> Bool) -> [a] -> Bool
all' f lst = length (filter (\y -> y) (map f lst)) == length lst

mean :: Integral a => [a] -> a
mean lst =
  let
    (first, second) = foldl (\(sum', length') x -> (x + sum', 1 + length')) (0, 0) lst
    in
    first `div` second

-- mean' :: Integral a => [a] -> a
-- mean' (x:xs) = total (x:xs)
mean' :: Integral a => [a] -> (a, a)
mean' [] = (0, 0)
mean' lst =
  let
    (a,b) = mean' (tail lst)
  in
