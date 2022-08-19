-- Lista por compreensão
-- [2*n |n<- [2,4,7]]
listaQuad = [x^2|x<-[1..30]]

listaQuadInf = [x^2|x<-[1..]]
--elem 4 listaQuadInf(verifica se 4 está contido na listaQuadInf)

listaQuadPares = [x^2 | x<-[1..20],even x]

listaQuadParesSup = [x^2 | x<-[1..20],even x,x>6]

listaPares = [even x| x<-[1..20]]

listaXvezY = [x*y| x<-[1,2,3],y<-[3,7,9]]

listaXeY = [(x,y)|x<-[1,3,5], y<- [2,4,6],x<y]

listaXeY2 = [(x,y)|x<-[1..3], y<- [1..3]]

listaXeY3 = [(x,y)|x<-[1..3], y<- [x..3]]

--Compreensão 
dobraPos :: [Int] -> [Int]
dobraPos [] = []
dobraPos xs = [2*x|x<-xs,x>0]

--Recursiva 
dobraPos2 :: [Int] -> [Int]
dobraPos2 [] = []
dobraPos2 (a:as) = if a>0 then 2*a:dobraPos2 as
                          else dobraPos2 as


fatores n = [i|i<-[1..n],mod n i ==0]
primo n = if (fatores n)==[1,n] then True
                                else False
