atividade = "3"
nome = "Victor Martins Vieira"
matricula = "508578"

-- Recebe uma string s e retorna uma lista de tuplas (c, f) onde c é umcaractere em s e f é o total de vezes que c ocorre em s. Exemplo, 

-- >> tls "aabcbaccc"
-- [('a', 3), ('b', 2), ('c', 4)]

-- OBS: a ordem das tuplas na lista de saída não é importante.
tls :: String -> [(Char, Int)]
tls = countOccurrences


-- Recebe uma string s de entrada e retorna uma tupla do tipo (p,f) onde p é a palavra mais frequente em s e f o valor de quantas vezes ela ocorre. Exemplo,

-- >> sfq "a casa. Ela casa. casa! 
-- ("casa", 3)

-- OBS:  Note que tokens como ponto e exclamação adjacentes as palavras não devem interferir na contagem.
sfq :: String -> (String, Int)
sfq s = getMax (countWordTotal (words (removeNonLetters s)))
    where
        getMax [word] = word
        getMax (wordX:wordX':wordsList) | snd wordX > snd wordX' = getMax (wordX:wordsList)
            | otherwise = getMax (wordX':wordsList)

        countWordTotal [] = []
        countWordTotal (wordX:wordsInput) = (wordX, count wordX wordsInput + 1) : countWordTotal (filter (/= wordX) wordsInput)
        count c = length . filter (== c)

        isLetter c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || (c == ' ')
        removeNonLetters = filter isLetter

--Funcoes auxiliares
countOccurrences :: String -> [(Char, Int)]
countOccurrences [] = []
countOccurrences (x:xs) = (x, count x xs + 1) : countOccurrences (filter (/= x) xs)
    where
        count c = length . filter (== c)
