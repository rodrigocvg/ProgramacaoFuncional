--Programador: Rodrigo Castro Vieira Gomes
--Data: 09/08/2021

--1-
--Definindo as funções saudacaoLegal e saudacaoInfeliz.

saudacaoLegal :: String
saudacaoLegal = "Ola! Que bom encontrar voce,"
saudacaoInfeliz :: String
saudacaoInfeliz= "Nao pensei que ainda estivesse vivo, "

--Definindo a função saudacao fazendo uso de guardas.

saudacao :: String -> String
saudacao nome
 |nome == "Joana" = saudacaoLegal ++ "Joana"
 |nome == "Fernando" = saudacaoLegal ++ "Fernando"
 |otherwise = saudacaoInfeliz ++ nome

--2- Função que, dados três coeficientes a, b, e c, informe quantas raízes a equação possui. 

funcaosegg :: Float -> Float -> Float -> String
funcaosegg a b c
 |delta>0 = "duas raizes reais"
 |delta==0 = "uma raiz real"
 |delta<0 =  "nao existem raizes reais"
   where 
    delta = (b*b)-4*a*c
  
--3- Função que recebe dois inteiros e retorna-os como um par ordenado.
ordena2 :: Int -> Int -> (Int, Int)
ordena2 x y
 |x>y = (y,x)
 |otherwise = (x,y)
 
--4-Função par que recebe um número inteiro e devolve verdadeiro (True) se o número for par e falso (False), caso contrário.
par :: Int -> Bool
par x
 |(mod x 2)==0 = True
 |otherwise = False
 
--5-Função impar que recebe um número inteiro e devolve verdadeiro (True) se o número for impar e falso (False), caso contrário.
impar :: Int -> Bool
impar x = not (par x) 