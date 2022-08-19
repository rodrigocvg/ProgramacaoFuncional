--Programador: Rodrigo Castro
--07/09/21


-- função recursiva conta_digitos que recebe um numero inteiro n e retorna sua quantidade de dígitos.
conta_digitos :: Int->Int
conta_digitos n 
 |n>=0 &&n<10 = 1
 |otherwise =(conta_digitos (div n 10)) + 1

-- função recursiva soma_digitos que recebe um numero inteiro n e retorna a soma de seus dígitos
soma_digitos :: Int -> Int
soma_digitos 0 = 0
soma_digitos x 
 |(div (x- mod x 10)10) <= 0 = mod x 10
 |otherwise = mod x 10 + soma_digitos (div (x- mod x 10) 10)



--função recursiva potencia (b, e) :: (Int, Int) ­> Int que eleva a base b ao expoente e.
potencia :: (Int,Int) -> Int
potencia (b,e) 
 |e==0 = 1
 |otherwise = b* potencia (b,e-1)
 

 
-- função ackermann 
ackermann :: (Int,Int) -> Int
ackermann (0,n) = n+1
ackermann (m,0) = ackermann (m-1,1)
ackermann (m,n) = ackermann(m-1, ackermann(m,n-1))
 
