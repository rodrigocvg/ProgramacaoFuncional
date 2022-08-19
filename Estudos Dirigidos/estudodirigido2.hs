--Programador: Rodrigo Castro Vieira Gomes
--30/09/2021


type Nome = String
type Preco = Int
type CodBar = Int

 

listaDeProdutos :: BaseDeDados
listaDeProdutos = [
                   (1234, "Oleo DoBom, 1l", 195),
                   (4756, "Chocolate Cazzeiro, 250g" , 180),
                   (3216, "Arroz DoBom, 5Kg", 213),
                   (5823, "Balas Pedregulho, 1Kg" , 379), 
                   (4719, "Queijo Mineirim, 1Kg" , 449),
                   (6832, "Iogurte Maravilha, 1Kg" , 499),
                   (1112, "Rapadura QuebraDente, 1Kg", 80),
                   (1111, "Sal Donorte, 1Kg", 221),
                   (1113, "Cafe DoBom, 1Kg", 285),
                   (1115, "Biscoito Bibi, 1Kg", 80),
                   (3814, "Sorvete QGelo, 1l", 695)
				   ]

type ListaDeCodigos = [CodBar]
type Recibo = [(Nome,Preco)]

tamLinha :: Int
tamLinha = 30


formataCentavos :: Preco -> String
formataCentavos x
 |rem x 10 <10 && x<10 = show ((div) x 100)  ++ "." ++ "0" ++ show ((rem) x 100)
 |otherwise = show ((div) x 100)  ++ "." ++ show ((rem) x 100)


inttostring :: Preco -> Nome
inttostring x = show x

formataLinha :: (Nome,Preco) -> String
formataLinha (x,y) = (x)++ ((replicate)(tamLinha -(length x)-(length(inttostring y)+1)) '.')++(formataCentavos y) ++ "\n"


formataLinhas :: [(Nome,Preco)] -> String
formataLinhas [] = []
formataLinhas (x:xs) = formataLinha x ++ formataLinhas xs

geraTotal :: Recibo -> Preco 
geraTotal [] = 0
geraTotal (x:xs) = snd x + geraTotal (xs)

formataTotal :: Preco -> String
formataTotal x = "Total" ++ ((replicate)(tamLinha - (length(formataCentavos x)+6)) '.') ++ "$" ++ (formataCentavos x) 

formataRecibo :: Recibo -> String
formataRecibo [] = []
formataRecibo x = formataLinhas x ++ formataTotal (geraTotal (x)) 

achaItem :: CodBar -> (Nome,Preco)
achaItem 1234 = ("Oleo DoBom, 1l", 195)
achaItem 4756 = ("Chocolate Cazzeiro, 250g" ,180)
achaItem 3216 = ("Arroz DoBom, 5Kg", 213)
achaItem 5823 = ("Arroz DoBom, 5Kg", 213)
achaItem 4719 = ("Queijo Mineirim, 1Kg" , 449)
achaItem 6832 = ("Iogurte Maravilha, 1Kg" , 499)
achaItem 1112 = ("Rapadura QuebraDente, 1Kg", 80)
achaItem 1111 = ("Sal Donorte, 1Kg", 221)
achaItem 1113 = ("Cafe DoBom, 1Kg", 285)
achaItem 1115 = ("Biscoito Bibi, 1Kg",80)
achaItem 3814 = ("Sorvete QGelo, 1l", 695)

fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo [] = [] 
fazRecibo (x:xs) = achaItem x:fazRecibo xs

geraRecibo :: ListaDeCodigos -> String
geraRecibo lc = formataRecibo(fazRecibo lc)


main = putStrLn (geraRecibo([1234,4756,3216,5823,4719,6832,1112,1111,1113,1115,3814]))


