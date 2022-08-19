import Data.Char 





{-f :: [Char] -> Bool
f ['a',_,_] = True
f _ = False
-}

f :: [Char] -> Bool
f ('a':_) = True
f _ = False
-- devolve a cabeça de uma lista
cab :: [Int] -> Int
cab (x:_) = x


--devolve a cauda de uma lista
cau :: [Int] -> [Int]
cau (_:xs) = xs

--vazia verifica se uma lista é vazia
vazia :: [Int] -> Bool
vazia [] = True
vazia _ = False

--calculo do produto dos elemtnos de uma lista
produto1 :: [Float] -> Float
produto1 [] = 1
produto1 (x:xs) = x * produto1 xs


produto2 :: [Float] -> Float
produto2 [x] = x
produto2 (x:xs) = x*(produto2 xs)
{-passo a passo da execução de produto1 [1,2,3]
Main> produto1 [1.0,2.0,3.0]
casa x com 1.0 e xs casa com [2.0,3.0]
==> deve calcular 
1.0*(produto1 [2.0,3.0])
x casa com 2.0 e xs casa com [3.0
==> deve calcular 
1.0* (2.0*produto [3.0])
x casa com 3.0 e xs com []
==> deve calcular 
1.0* (2.0 *(3.0*produto1 [])
1.0 * 2.0 *3.0
1.0 * 6.0
6.0
-}


--comprimento de uma string
comprimento :: String -> Int
comprimento [] = 0
comprimento (_:xs) = 1 + comprimento xs

--devolve o num de elementos de uma lista
nro_elementos :: [Int] -> Int
nro_elementos [] = 0
nro_elementos (x:xs) = (nro_elementos xs) + 1

--devolve o reverso de uma string
reverso :: String -> String
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

separa :: [Int] -> [Int]
separa (p:s:r) = (s:r)


{-separab :: [Int] -> [Int]
separab (p:s:r) =  (p:s:r)
-}

separac :: [Int] ->  [Int] 
separac (p:r) = r

comprimentol :: [Int] -> Int
comprimentol [] = 0
comprimentol (x:xs) = 1 + (comprimentol xs)

somatorio :: [Int] -> Int
somatorio [] = 0
somatorio (x:xs) = x + (somatorio xs)

somatorio_impares :: [Int] -> Int
somatorio_impares [] = 0
somatorio_impares (x:xs)
 |mod x 2 /= 0 = x + somatorio_impares xs
 |otherwise = somatorio_impares xs

soma_quadrado :: [Int] -> Int
soma_quadrado [x] = x^2
soma_quadrado (x:xs) = x^2 + (soma_quadrado xs)

{-n_esimo :: Int->[Int]->Int
n_esimo 0 (x:xs) = x
n_esimo y (x:xs) = n_esimo y (x:xs) + 1 + y

-}
n_esimo :: Int -> [Int] -> Int
n_esimo 0 (x:xs) = x
n_esimo n (x:xs) = n_esimo (n-1) xs

duplica :: [Int] -> [Int]
duplica [] = []
--duplica (x:xs) =  [x,x] ++ (duplica xs) 
duplica (x:xs) =  x:x:(duplica xs) 

reverso1 :: [Int] -> [Int]
reverso1 [] = []
reverso1 (x:xs) = reverso1 xs ++ [x]

substituir_todos :: Int -> Int -> [Int] -> [Int]
substituir_todos _ _ [] = []
substituir_todos v1 v2 (x:xs) 
 |x==v1 = v2:(substituir_todos v1 v2 xs)
 |otherwise = x:(substituir_todos v1 v2 xs)
 
maior :: [Int] -> Int
maior [x] = x
maior (x:xs) 
 |x>=y = x
 |otherwise = y
  where
   y= maior xs
   
desduplica :: [Int] -> [Int]
desduplica [] = []
desduplica (x:_:xs) =  x:(desduplica xs)

insere :: Int -> [Int] -> [Int]
insere n [] = [n]
insere n (x:xs)
 |n<=x = (n:x:xs)
 |otherwise = x:(insere n xs)

ordena :: [Int] -> [Int]
ordena [] = []
ordena (x:xs) =   insere x (ordena xs)

pertence :: Int -> [Int] -> Bool
pertence n [] = False
pertence n (x:xs)
 |n==x = True
 |otherwise = pertence n xs
 
 
concatenar :: [[Int]] -> [Int] 
concatenar [] = []
concatenar (x:xs) = x ++ (concatenar xs)

uniao :: [Int] -> [Int] -> [Int]
uniao [] (x:xs) = (x:xs)
uniao (x:xs) (y:ys) 
 |not (pertence x (y:ys)) = x:(uniao xs (y:ys))
 |otherwise = uniao xs (y:ys)

subc :: [Int] -> [Int] -> Bool
subc [] (x:xs) = True
subc (y:ys) (x:xs) 
 |pertence y (x:xs) = (subc ys (x:xs))   
 |otherwise = False 

maius :: String -> String
maius [] = []
maius (x:xs) = toUpper x:(maius xs)

maius2 :: String -> String
maius2 [] = []
maius2 (x:xs) 
 |isAlpha x = (toUpper x):(maius xs)
 |otherwise = maius2 xs
 
soma :: Int -> [Int] -> Int
soma _ [] = 0
soma n (x:xs) = x*(2^n) + soma (n-1) xs
 
--converte :: [Int] -> Int
--converte 


