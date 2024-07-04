-- https://www.dikastis.com.br/problems/01EKZCVAB3YD84D72FV98A6Y0R ACCEPTED
unicos :: [Int] -> [Int]
unicos [] = []
unicos (x : xs)
    | x elem xs = unicos [a | a <- xs, a /= x]
    | otherwise = x : unicos xs

-- https://www.dikastis.com.br/problems/01FATVJV9GAHC921MZQA5EKMKR ACCEPTED
-- índices pares na primeira lista e índices ímpares na segunda lista

metade :: [a] -> ([a], [a])
metade lista = divide lista 0 ([], [])
  where
    divide :: [a] -> Int -> ([a], [a]) -> ([a], [a])
    divide [] _ (esq, dir) = (reverse esq, reverse dir)
    divide (x:xs) i (esq, dir)
      | n`mod` 2 == 0    = divide xs (i + 1) (x:esq, dir)
      | otherwise = divide xs (i + 1) (esq, x:dir)


-- https://www.dikastis.com.br/problems/01FATTFW5RQQDNQTGY4VYBYWWM ACCEPTED
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] x = x
merge x [] = x
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort lst = merge (msort metade1) (msort metade2)
  where
    (metade1, metade2) = splitAt (div (length lst) 2) lst

msort :: Ord a => [a] -> [a]
msort [] = []
msort lst = merge (msort metade1) (msort metade2)
    where 
        (metade1, metade2) = splitAt (div (length lst) 2) lst
merge :: [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
mrge (x:xs) (y:ys)
    | x <= y = x:merge xs (y:ys) 
    | otherwise = y : merge (x:xs) ys

-- https://www.dikastis.com.br/problems/01EKZG284CN1K3WQQ7YRC0RFQR ACCEPTED
remDiv :: Int -> [a] -> ([a], [a])
remDiv 1 (_ : ys) = ([], ys) -- Ainda tem lista sobrando e vai remover o elemento
remDiv n [] = ([], []) -- Acabou a lista e ainda não removeu o elemento
remDiv n (x : xs) = (x : r1, r2)
  where
    (r1, r2) = remDiv (n - 1) xs

-- https://www.dikastis.com.br/problems/01EMPA7NS2AQKZCWJ44M4HXGK3 ACCEPTED
somaSqrt :: [Double] -> Double
somaSqrt lst = foldr (+) 0 (map sqrt (filter (> 0) lst))

-- https://www.dikastis.com.br/problems/01EJ1TJE182TZKHBXQRE172WA6 ACCEPTED
-- B(n, k) = B(n − 1, k) + B(n − 1, k − 1)
-- B(n, 0) = 1
-- B(0, k) = 0, quando k > 0
binomial :: Int -> Int -> Int
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n - 1) k + binomial (n - 1) (k - 1)