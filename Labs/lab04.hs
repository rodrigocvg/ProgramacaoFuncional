--1
terceiro :: String -> Char
terceiro s = head (tail(tail s))

--2
--a
ultimo :: String -> Char 
ultimo s = head (reverse s)


--b
inicio :: String -> String
inicio s =  reverse (tail (reverse s))
 

--3 
iniciais :: String -> String -> (Char, Char)
iniciais a b = (head a, head b)

