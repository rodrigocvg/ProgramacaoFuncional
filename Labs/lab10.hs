--Polimorfismo 


primeiro :: [a] -> a
primeiro (x:xs) = x

somatorio :: Num t => [t] -> t
somatorio [] = 0
somatorio (x:xs) =  x + somatorio xs


par :: Integral t => t -> Bool
par x = mod x 2 == 0


metade :: Fractional t => t -> t
metade x = x / 2


antes :: Ord t => (t, t) -> Bool
antes (x, y) = x < y

iguais :: Eq t => (t, t) -> Bool
iguais (x, y) = x == y


segundo :: [a] -> a
segundo xs = head (tail xs)

trocar :: (a,b) -> (b,a)
trocar (x,y)  = (y,x)

parear :: a -> b -> (a,b)
parear x y = (x,y)

dobro ::Num a => a -> a
dobro x = x*2

palindromo :: Eq a => [a] -> Bool
palindromo xs = reverse xs == xs

terceiro :: (a,a,a) -> a
terceiro (x,y,z) = z

iguais2 :: Eq a => a -> a -> a -> Int
iguais2 x y z
 |x==y && y==z = 3
 |x==y && y/=z = 2
 |y==z && z/=x = 2
 |x==z && z/=y = 2
 |otherwise = 0
 
somaimp :: Integral a => [a] -> a
somaimp [] = 0
somaimp (x:xs)
 |mod x 2/=0 = x + somaimp xs
 |otherwise = somaimp xs
 
pertence :: Eq a => a -> [a] -> Bool
pertence n [] = False
pertence a (x:xs)
 |a==x = True
 |otherwise = pertence a xs