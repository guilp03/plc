import System.IO

-- Definindo a função poli e listaPoli como exemplo
poli :: Integer -> Integer -> Integer -> Integer -> Integer
poli a b c x = a * x * x + b * x + c

listaPoli :: [(Integer, Integer, Integer)] -> [Integer -> Integer]
listaPoli l = [poli a b c | (a, b, c) <- l]

-- Definindo a função appListaPoli sem usar zipWith
appListaPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListaPoli [] _ = []  -- Caso base: se a lista de funções estiver vazia, retorna uma lista vazia
appListaPoli (f:fs) (x:xs) = f x : appListaPoli fs xs
appListaPoli _ _ = []  -- Caso padrão, apenas retorna uma lista vazia

-- Função principal para execução do programa
main :: IO ()
main = do
    let polynomials = [(\x -> x^2 + 2*x + 1), (\x -> 2*x^2 + 3*x + 4)]
    let values = [1, 2, 3]
    let resultados = appListaPoli polynomials values
    -- Imprime os resultados usando putStrLn para garantir que apareça na tela
    putStrLn "Resultados:"
    print resultados
