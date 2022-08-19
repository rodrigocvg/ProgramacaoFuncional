
somaN :: Int -> Int
somaN n 
 |n == 0 = 0
 |otherwise = n + somaN (n-1)

mdc :: Int -> Int -> Int
mdc m n
 |n==0 = m
 |n>0 = mdc n (mod m n)
 

{-utilizando casamento de padrões
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b 
 |b>0 = mdc b (mod a b)
 |otherwise = error "o valor do segundo argumento deve ser maior que zero"
-}

mdc_3 :: Int -> Int -> Int -> Int
mdc_3 a b c = mdc (mdc a b) c 
 
mmc :: Int -> Int -> Int
mmc m n = div (m*n) (mdc m n)

soma_intervalo :: Int -> Int -> Int 
soma_intervalo n1 n2 
 |n1==n2 = n1
 |n1<n2 = (soma_intervalo n1 (n2-1)) + n2
 |otherwise = (soma_intervalo n2 (n1-1)) + n1
 
comb :: Int -> Int -> Int 
comb n k
 |k==1 = n
 |n==1 = k
 |(k>1) && (n<k) = (comb (n-1) (k-1)) + (comb (n-1) (k)) 
 |otherwise = error "valor de k digitado incorretamente"

fatorial :: Float -> Float
fatorial n
 |n == 0 = 1
 |otherwise = n * fatorial (n-1)

expon :: Float -> Float -> Float
expon x n 
 |n==0 = 1
 |otherwise = (expon x (n-1)) + ((x**n)/(fatorial n))
 {-
 fatorial :: Int -> Int
fatorial n
 |n == 0 = 1
 |otherwise = n * fatorial (n-1)
 
 expon :: Float -> Int -> Float
expon x n 
 |n==0 = 1
 |otherwise = (expon x (n-1)) + ((x^n)/fromIntegral (fatorial n))
 -}

 
