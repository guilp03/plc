primos :: [Int] -> [Int]
primos [x] = x
primos (x:xs)
| divisor x xs = True = primos xs
| otherwise = (x:primos xs)

primosN :: Int -> [Int]
primosN x = primos [1..x]

divisor :: Int -> [Int] -> Bool
divisor x [] = True
divisor x (y:ys)
| x `mod` y == 0 = False
| otherwise = divisor x ys

