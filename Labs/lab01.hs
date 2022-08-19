--primeiro programa haskell
dobro x = 2 * x
quadruplo x = dobro (dobro x)
soma2 x y = x + y
soma4 x y z w = x + y + z + w
misterio x y z w = soma2 (soma2 x y) (soma2 z w)
hipotenusa x y = sqrt ( x^2 + y^2)
maior_valor :: Int
maior_valor = 2^(64-1) - 1
menor_valor :: Int
menor_valor = -2^(64-1)