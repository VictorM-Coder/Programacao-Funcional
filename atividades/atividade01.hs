atividade = "1"

-- 1
-- Recebe uma string e 
-- retorna-a sem as vogais.   
noVog :: String -> String
noVog s = [x | x <- s, x `notElem` "aeiouAEIOU"]

-- 2
-- retorna quantas vezes x é divisível por n
num'divs :: Int -> Int -> Int
num'divs x n = if x `mod` n == 0
    then 1 + num'divs (x `div` n) n
    else 0

-- 3
-- Dado um inteiro n. determinar se
-- ele é ou não um número primeo
is'prime :: Int -> Bool
is'prime n = aux'primo n (floor (sqrt (fromIntegral n)))


-- 4
-- inverte um inteiro, por exemplo
-- o inverso de 251 é 152.
int'inv :: Int -> Int
int'inv x = read (reverse (show x))


-- Funcoes auxliares
aux'primo:: Int -> Int -> Bool
aux'primo dividendo divisor
  | dividendo <= 1 = False
  | divisor == 1 = True
  | dividendo `mod` divisor == 0 = False
  | otherwise = aux'primo dividendo (divisor-1)
