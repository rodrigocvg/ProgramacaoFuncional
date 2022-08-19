

calcula :: Char -> Int -> Int -> Int
calcula x y z
 |x=='*' = y*z
 |x=='/' = div y z
 |otherwise = error "erro!"
