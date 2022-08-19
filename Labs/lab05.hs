menor :: (Int, Int) -> Int
menor (x,y)
 |x<=y = x
 |x>y = y              --otherwise tambem serve

menor3 :: (Float,Float,Float) -> Float
menor3 (x,y,z) 
 |x<=y && x<=z = x
 |y<=x && y<=z = y
 |otherwise = z
 
 
qtsiguais :: (Float,Float,Float) -> Int
qtsiguais (a,b,c)
 |(a==b) && (b==c) = 3
 |(a==b) || (b==c) || a==c  = 2
 |otherwise = 0
 
maiormenor :: (Int,Int,Int) -> (Int,Int)
maiormenor (x,y,z)
 |x<=y && x<=z && y>z = (y,x)
 |x<=y && x<=z && z>y = (z,x)
 |y<=x && y<=z && x>z = (x,y)
 |y<=x && y<=z && z>x = (z,y)
 |z<=x && z<=y && x>y = (x,z)
 |z<=x && z<=y && y>x = (y,z)
 
funcaosegg :: Float -> Float -> Float -> Float
funcaosegg a b c
 |(b^2)>4*a*c = 2
 |(b^2)==4*a*c = 1
 |(b^2)<4*a*c = 0
 
datas :: (Int,Int,Int) -> (Int,Int,Int) -> String
datas (d1,m1,a1) (d2,m2,a2)
 |a1==a2 && m1==m2 && d1>d2 = "Segunda data ocorreu antes da Primeira"
 |a1==a2 && m1==m2 && d2>d1 = "Primeira data ocorreu antes da segunda"
 |a1==a2 && m1>m2 = "Segunda data ocorreu antes da Primeira"
 |a1==a2 && m2>m1 = "Primeira data ocorreu antes da segunda"
 |a1>a2 = "Segunda data ocorreu antes da Primeira"
 |a2>a1 = "Primeira data ocorreu antes da segunda"
 
ordena2 :: Int -> Int -> (Int, Int)
ordena2 x y
 |x>y = (y,x)
 |otherwise = (x,y)
 
par :: Int -> Bool
par x
 |(mod x 2)==0 = True
 |otherwise = False

impar :: Int -> Bool
impar x = not (par x) 

f :: (Float,Float) -> Float
f (x,y) = (a+1) * (b+2)
 where
  a= (x+y)/2
  b= (x+y)/3
  
analisaIMC :: Float -> Float -> String
analisaIMC peso altura
 |imc<=18.5= "Voce esta abaixo do peso!"
 |imc<=25.0= "Peso normal"
 |imc<=30.0= "Voce esta acima do peso!"
 |otherwise= "Voce está obeso!"
  where
   imc = peso/altura^2  
   
saudacaoLegal :: String
saudacaoLegal = "Ola! Que bom encontrar voce,"


saudacao :: String -> String
saudacao "Joana" = saudacaoLegal ++ "Joana"
saudacao "Fernando" = saudacaoLegal ++ "Fernando"
saudacao nome = saudacaoInfeliz ++ nome
 where
  saudacaoInfeliz= "Nao pensei que ainda estivesse vivo, "

areaSuperfCil :: Double -> Double -> Double
areaSuperfCil r h =
 let
 areaLado = 2*pi*r*h
 areaBase = pi*r^2
 in areaLado+2*areaBase
 
 


 
 
