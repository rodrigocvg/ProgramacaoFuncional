import Data.Char
dobrar :: Num a => [a] -> [a]
dobrar l = map (*2) l

incrementar :: Num a => [a] -> [a]
incrementar l = map (+1) l

pega_letras :: String -> String
pega_letras xs = filter isAlpha xs

pega_digitos :: String -> String
pega_digitos xs = filter isDigit xs

--função foldr