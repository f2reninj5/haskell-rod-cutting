rod :: [Int] -> Int -> Int
max' :: Ord a => a -> a -> a
lmax :: Ord a => [a] -> a

max' x x'
    | x > x' = x
    | otherwise = x'

lmax [] = error "empty list"
lmax [x] = x
lmax (x:xs) = foldl max' x xs

rod _ 0 = 0
rod xs n = lmax [xs!!(i - 1) + (rod xs (n - i)) | i <- [1..n]]

r :: Int -> Int
r = rod [1,5,8,9,10,17,17,20,24,30]
