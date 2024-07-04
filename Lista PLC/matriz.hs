matriz :: [[a]] -> Bool
matriz [] = True  -- Lista vazia é uma matriz válida
matriz [x] = True  -- Uma única linha é sempre uma matriz válida
matriz (x:y:ys) = length x == length y && matriz (y:ys)

permutation :: Int -> Int -> [[a]] -> [[a]]
permutation x y matriz
    | x < y && x >= 0 && y < length matriz = inicio ++ [linhaY] ++ meio ++ [linhaX] ++ fim
    | otherwise = matriz
    where
        linhaX = matriz !! x       
        linhaY = matriz !! y        
        inicio = take x matriz  
        meio = take (y - x - 1) (drop (x + 1) matriz)
        fim = drop (y + 1) matriz 
