sublista :: [a] ->[[a]]
sublista [] = [[]]
sublista [a] = [[a]]
sublista (x:xs) = [x:ys | ys <- sublista xs] ++ sublista xs

sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = sublistas xs ++ [x:sublist | sublist <- sublistas xs]
