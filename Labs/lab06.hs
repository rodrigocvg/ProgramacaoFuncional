{-
not_logico :: Bool -> Bool
not_logico True = False
not_logico False = True
-}

not_logico :: Bool -> Bool
not_logico x
 |x = False                   --ou x==True = False (msm coisa)
 |otherwise = True
 
{-
e_logico :: (Bool,Bool) -> Bool
e_logico (True,True) = True
e_logico (True,False) = False
e_logico (False,True) = False
e_logico (False,False) = False
-}

e_logico :: (Bool,Bool) -> Bool
e_logico (x,y)
 |x&&y = True        -- ou x==True&&y==True = True       
 |otherwise = False
 
{-
e_logico' :: (Bool,Bool) -> Bool
e_logico' (True,True) = True
e_logico' (_,_) = False



e_logico :: (Bool,Bool) -> Bool
e_logico (True,b) = b
e_logico (False,_) = False


e_logico_errado :: (Bool,Bool) -> Bool
e_logico_errado (_,_) = True
e_logico_errado (True,True) = False

e_logico_errado’ :: (Bool,Bool) -> Bool
e_logico_errado’ (b,b) = b
e_logico_errado’ (_,_) = False
-}


-- Definir a função ou lógico utilizando casamento de padrões

{-
ou_logico :: (Bool,Bool) -> Bool
ou_logico (True,True) = True
ou_logico (True,False) = True
ou_logico (False,True) = True
ou_logico (False,False) = False


ou_logico :: (Bool, Bool) -> Bool
ou_logico (False, False) = False 
ou_logico (_,_)          = True

-}


ou_logico :: (Bool,Bool) -> Bool
ou_logico (x,y)
 |(x==False) && (y==False) = False
 |otherwise = True

{-
scheffer :: Bool -> Bool -> Bool
scheffer False False = True
scheffer _ _ = False 
-}

scheffer :: Bool -> Bool -> Bool
scheffer x y 
 |x&&y = False
 |otherwise = True
 
error :: String 

calcula :: String -> Int -> Int -> Int
calcula x y z
 |x=="*" = y*z
 |x=="/" = y/z
 |otherwise = error

