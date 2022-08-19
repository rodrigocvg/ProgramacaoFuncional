area_retangulo :: Int -> Int -> Int
area_retangulo x y = x*y
area_quadrado :: Int -> Int
area_quadrado x = x * x
area_triangulo :: Float -> Float -> Float
area_triangulo b h = (b*h)/2
area_trapezio :: Float -> Float -> Float -> Float
area_trapezio x y z = ((x+y)*z)/2
area_circulo :: Float -> Float
area_circulo r = pi*r*r


area_coroacircular :: Double -> Double -> Double
area_coroacircular x y = (pi*y*y) - (pi*x*x)
volume_cubo :: Int -> Int
volume_cubo x = x*x*x
volume_paralelepipedo :: Int -> Int -> Int -> Int
volume_paralelepipedo x y z = x*y*z
volume_piramide :: Float -> Float -> Float
volume_piramide x y = (x*y)/3
volume_esfera :: Double -> Double
volume_esfera x = (4*pi*x*x*x)/3
hipotenusa :: Float -> Float -> Float
hipotenusa x y = sqrt ( x^2 + y^2)
distancia_origem :: Float -> Float -> Float
distancia_origem x y = sqrt ((0-x)^2 + (0-y)^2)
distancia_doispontos :: Float -> Float -> Float -> Float -> Float
distancia_doispontos x y z w = sqrt ((z-x)^2 + (w-y)^2)
cubo_numero :: Int -> Int
cubo_numero x = x*x*x
quadrado :: Int -> Int
quadrado x = x*x
quarta_potencia :: Int -> Int
quarta_potencia x = (quadrado x) * (quadrado x)
horas :: Float -> Float
horas x = x/3600
minutos :: Float -> Float
minutos x = x/60
celsiusf :: Double -> Double
celsiusf x = (x-32)/1.8
celsiusk :: Double -> Double
celsiusk x = x-273
kelvinf :: Double ->  Double
kelvinf x = ((x-32)/9)*5
convertekmhms :: Float -> Float
convertekmhms x = x/3.6
logica1 :: Bool -> Bool -> Bool
logica1 p q = (p || q) && not (p && q)
logica2 :: Bool -> Bool -> Bool -> Bool
logica2 p q r = (p || q) && r
logica3 :: Bool -> Bool -> Bool -> Bool
logica3 p q r = (p && q) || not(p && r)
logica4 :: Bool -> Bool -> Bool -> Bool -> Bool
logica4 p q r s = p || (q && r) || not s
logica5 :: Bool -> Bool -> Bool -> Bool -> Bool
logica5 p q r s = not(p || q) && (r || s) && not r









