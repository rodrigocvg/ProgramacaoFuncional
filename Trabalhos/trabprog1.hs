-- Programador: Rodrigo Castro Vieira Gomes
-- Data: 16/10/2021

-- ********************************************************************** --
--UTILIZANDO RECURSIVIDADE: 

type Pessoa = String
type Livro = String
type Emprestimo = [(Pessoa,Livro)]

-- Banco de dados de empréstimos 
emprestimos :: Emprestimo
emprestimos = [("Joao","Dom Quixote"),
               ("Joao","Cem anos de solidao"),
			   ("Lucas","Jogos Vorazes"),
			   ("Maria","Santa Ceia"),
			   ("Matheus","O velho e o mar"),
               ("Israel","Moby Dick"),
               ("Catia","Cem anos de solidao"),
               ("Rodrigo","A menina que roubava livros"),
			   ("Joao","O estrangeiro"),
			   ("Tiago","Segunda Guerra Mundial"),
			   ("Catia","Segunda Guerra Mundial")]
			   
			   
--Função encontLivro que dado um nome e o banco de dados "emprestimos" na janela de fundo preto, devolve qual livro aquela pessoa realizou o empréstimo
encontLivro :: Pessoa -> Emprestimo -> [Livro]
encontLivro a [] = []
encontLivro a (x:xs)
 |a == fst (x) = snd (x):encontLivro a xs 
 |otherwise = encontLivro a xs

--Função encontPessoa que dado um livro e o banco de dados "emprestimos" na janela de fundo preto, devolve qual pessoa está com aquele livro emprestado
encontPessoa :: Livro -> Emprestimo -> [Pessoa]
encontPessoa a [] = []
encontPessoa a (x:xs) 
 |a == snd (x) = fst (x):encontPessoa a xs
 |otherwise = encontPessoa a xs
 
--Função pertence, que verifica se um livro pertence a lista de empréstimos
pertence :: Livro -> Emprestimo -> Bool
pertence n [] = False
pertence n (x:xs)
 |n == snd x = True
 |otherwise = pertence n xs

-- Função livFoiEmp, que dado um livro, utilizando a função pertence, verifica se ele está emprestado
livFoiEmp :: Livro -> String
livFoiEmp x 
 |pertence x emprestimos = "Livro esta sendo emprestado"
 |otherwise = "Livro nao esta sendo emprestado"
 
 
-- Função qntdLivEmprPess, que dado uma pessoa e a base de dados emprestimos, devolve a quantidade de livro que essa pessoa pegou emprestado
qntdLivEmprPess :: Pessoa -> Emprestimo -> Int
qntdLivEmprPess a [] = 0
qntdLivEmprPess a (x:xs) = length (encontLivro a emprestimos)



-- ************************************************************** --
-- ************************************************************** -- 
--UTILIZANDO COMPREENSÃO DE LISTAS

encontLivro2 :: Pessoa -> [Livro]
encontLivro2 a = [snd x | x <- emprestimos, fst x == a]

encontPessoa2 :: Livro -> [Pessoa]
encontPessoa2 a = [fst x | x <- emprestimos, snd x == a]

livFoiEmp2 :: Livro -> String
livFoiEmp2 x
 |pertence x emprestimos = "Livro esta sendo emprestado"
 |otherwise = "Livro nao esta sendo emprestado"

qntdLivEmprPess2 :: Pessoa -> Int
qntdLivEmprPess2 x = length (encontLivro2 x)
 


	




