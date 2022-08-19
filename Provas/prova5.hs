--Rodrigo Castro
--13/10/2021

type Produto = String
type Preco = Int

formataCentavos :: Preco -> String
formataCentavos x
 |rem x 10 <10 && x<10 = show ((div) x 100)  ++ "." ++ "0" ++ show ((rem) x 100)
 |otherwise = show ((div) x 100)  ++ "." ++ show ((rem) x 100)


escreveLinha :: (Produto,Preco) -> String
escreveLinha (x,y) = (x)++((replicate)(30 -(length x)-(length(show y))) '.')++(formataCentavos y) 