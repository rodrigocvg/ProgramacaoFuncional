				--Programador: Rodrigo Castro Vieira Gomes
--14/10/2021


tamLinha :: Int
tamLinha = 30


formataCentavos ::(Integral x,Show x) => x -> String
formataCentavos x
 |mod x 10 <10 && x<10 = show ((div) x 100)  ++ "." ++ "0" ++ show ((mod) x 100)
 |otherwise = show ((div) x 100)  ++ "." ++ show ((mod) x 100)


inttostring ::(Integral x,Show x) => x -> String
inttostring x = show x


formataLinha :: ((Integral x),Show x)=>(String,x) -> String
formataLinha (x,y) = (x)++ ((replicate)(tamLinha -(length x)-(length(inttostring y)+1)) '.')++(formataCentavos y) ++ "\n"


formataLinhas :: (Integral x,Show x) => [(String,x)] -> String
formataLinhas [] = []
formataLinhas (x:xs) = formataLinha x ++ formataLinhas xs


geraTotal :: Num b => [(a,b)] -> b 
geraTotal [] = 0
geraTotal (x:xs) = snd x + geraTotal (xs)


formataTotal :: (Integral x,Show x) => x -> String
formataTotal x = "Total" ++ ((replicate)(tamLinha - (length(formataCentavos x)+6)) '.') ++ "$" ++ (formataCentavos x) 


formataRecibo :: (Integral x,Show x) => [(String,x)] -> String 
formataRecibo [] = []
formataRecibo x = formataLinhas x ++ formataTotal (geraTotal (x)) 


achaItem :: (Integral x) => x -> (String,x)
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


fazRecibo :: (Integral x,Show x) => [x]-> [(String, x)]
fazRecibo [] = [] 
fazRecibo (x:xs) = achaItem x:fazRecibo xs


geraRecibo :: (Integral x,Show x) => [x] -> String
geraRecibo lc = formataRecibo(fazRecibo lc)

main = putStrLn (geraRecibo([1234,4756,3216,5823,4719,6832,1112,1111,1113,1115,3814]))
