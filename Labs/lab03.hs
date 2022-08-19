--Tipos Compostos de Dados
--Tuplas
--modiv
modiv :: (Int,Int) -> (Int, Int)
modiv (x,y) = (div x y, mod x y)

segundos :: (Int,Int,Int) -> Int
segundos (x,y,z) = x*3600 + y*60 + z

horario :: Int -> (Int,Int,Int)
horario x = ((div x 3600), (div (mod x 3600) 60), (mod x 60))
