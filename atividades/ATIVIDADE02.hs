atividade = "2"

-- 1 
-- Sejam as tuplas u e v de inteiros
-- tal que exista um inteiro k onde
-- u = kv ou v = ku  
-- então u e v são mútiplos. Construa 
-- função que determine se duas 
-- tuplas de inteiros  são múltiplas.
isMult :: (Int,Int) -> (Int, Int) -> Bool
isMult u v = (fst u * snd v) == (fst v * snd u)

-- 2
-- Sejam todos os triângulos retângulos
-- de perímetro p e de lados inteiros.
--   representados por tuplas (a,b,c) 
-- com  a>=b>=c. Criar  
--  função que determine 
-- o total destes triângulos dado p .
tot'tri  :: Int -> Int
tot'tri p = length [(a, b, c) |  b <- [1..(div p 2)], c <- [1..b], let a = p-b-c, a*a==b*b+c*c]


