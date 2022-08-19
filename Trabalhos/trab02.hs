-- Programador: Rodrigo Castro Vieira Gomes
-- Data: 02/08/2021	

-- Função terceiro que devolve o terceiro elemento de uma lista de inteiros.
terceiro :: String -> Char
terceiro s = head (tail(tail s))


--Função ultimo, que devolve o ultimo elemento de uma string.
ultimo :: String -> Char 
ultimo s = head (reverse s)


--Função inicio, que devolve todos os elementos da string, exceto o ultimo.
inicio :: String -> String
inicio s =  reverse (tail (reverse s))
 

--função que receba o primeiro e o último nome de alguém e retorne suas iniciais em uma tupla.
iniciais :: String -> String -> (Char, Char)
iniciais a b = (head a, head b)
