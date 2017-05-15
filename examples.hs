
take' :: (Num a, Ord a) => a -> [a] -> [a]
take' n _ 
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let
        left = quicksort' [a | a <- xs, a <= x]
        right = quicksort' [b | b <- xs, b > x]
    in
        left ++ [x] ++ right

dubApp :: (a -> a) -> a -> a
dubApp f x = f (f x)