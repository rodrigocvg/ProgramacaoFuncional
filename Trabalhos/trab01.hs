-- Definindo as funções
-- 1-
inc :: Int -> Int
inc x = x + 1
quadrado :: Int -> Int
quadrado x = x * x
media :: Float -> Float -> Float
media a b = (a + b) / 2.0

-- 2-
quarta_potencia :: Int -> Int                   --função que determine a quarta potência de um número n (n4 ), usando a função que determina o quadrado de um número;
quarta_potencia x = (quadrado x) * (quadrado x)
-- 3-
horas :: Float -> Float                         --função que, dado um total de segundos, calcule o total de horas;
horas x = x/3600
-- 4-
minutos :: Float -> Float                       --função que, dado um total de segundos, calcule o total de minutos usando a função definida em 3.
minutos x = (horas x) * 60
-- 5- 




-- Valor das equações
-- 1
-- (a) inc (quadrado 5) = 26, pois quadrado de 5 é igual a 25, e a função inc soma +1 ao resultado, portanto 26.
-- (b) quadrado (inc 5) = 36, pois inc 5 é igual a 6, e a função quadrado calcula o quadrado do número, portanto 6*6 = 36
-- (c) media (inc 3)(inc 5) = deu erro, pois a função inc é do tipo Int, e a função media é do tipo float, o que levou a mensagem de erro no GHCi

