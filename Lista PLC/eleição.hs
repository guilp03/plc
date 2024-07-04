type Codigo = Int
type Urna = [Voto]
type Apuracao = [(Voto, Integer)]

data Voto = Presidente Codigo
          | Senador Codigo
          | Deputado Codigo
          | Branco
          deriving (Show)

instance Eq Voto where
    (Presidente cod1) == (Presidente cod2) = cod1 == cod2
    (Senador cod1) == (Senador cod2) = cod1 == cod2
    (Deputado cod1) == (Deputado cod2) = cod1 == cod2
    Branco == Branco = True
    _ == _ = False

totalVotos :: Urna -> Voto -> Int
totalVotos urna candidato = length $ filter (== candidato) urna

apuracao :: Urna -> Apuracao
apuracao urna = contarVotos urna []
  where
    contarVotos :: Urna -> Apuracao -> Apuracao
    contarVotos [] apuracaoAtual = apuracaoAtual
    contarVotos (voto:restoUrna) apuracaoAtual =
        contarVotos restoUrna (atualizarApuracao voto apuracaoAtual)
    
    atualizarApuracao :: Voto -> Apuracao -> Apuracao
    atualizarApuracao voto [] = [(voto, 1)]
    atualizarApuracao voto ((v, n):restoApuracao)
        | v == voto  = (v, n + 1) : restoApuracao
        | otherwise = (v, n) : atualizarApuracao voto restoApuracao

-- Exemplo de urna
urnaExemplo :: Urna
urnaExemplo = [Presidente 1, Senador 2, Presidente 1, Deputado 3, Presidente 2, Branco]

-- Exemplo de uso das funções
main :: IO ()
main = do
    putStrLn "Exemplo de Urna:"
    print urnaExemplo
    
    let presidente1 = Presidente 1
    putStrLn $ "\nTotal de votos para " ++ show presidente1 ++ ": " ++ show (totalVotos urnaExemplo presidente1)
    
    putStrLn "\nApuração completa:"
    print (apuracao urnaExemplo)
