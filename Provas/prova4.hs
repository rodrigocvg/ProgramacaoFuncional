--Rodrigo Castro Vieira Gomes
--15/09/2021

--função que obtem o n-ésimo termo da sequência, que soma os ultimos dois termos
termo_n :: Int -> Int
termo_n 1 = 0
termo_n 2 = 1
termo_n n = termo_n (n-1) + termo_n (n-2)