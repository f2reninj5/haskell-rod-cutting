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
rod (x:xs) 1 = x
-- rod xs n = foldl (\acc x -> max' acc (x + rod xs (n - 1))) 0 xs
-- rod xs n = foldl (\acc (y:ys) -> max' acc (y + rod ys (n - 1))) 0 xs
rod xs n = lmax [xs!!(i - 1) + (rod xs (n - i)) | i <- [1..n]]

-- rod [1,5] 2 = lmax [xs!!(i - 1) + (rod xs n - i) | i <- [1,2]]
-- rod [1,5] 2 = lmax [1 + (rod [1,5] 0), 5 + (rod [1,5] 1)]

r :: Int -> Int
r = rod [1,5,8,9,10,17,17,20,24,30]
