-- Programador: Rodrigo Castro Vieira Gomes
-- 24/10/2021


-- ********************************************************************** --


-- Banco de dados de empréstimos 
emprestimos :: [(String,String)]
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
			   
			   
--Função encontLivro que dado um nome,devolve qual livro aquela pessoa realizou o empréstimo, utilizando map e filter

encontLivro :: String -> [String]
encontLivro a = map snd (filter ((== a).fst) emprestimos)

--Função encontPessoa que dado um livro e o banco de dados "emprestimos" na janela de fundo preto, devolve qual pessoa está com aquele livro emprestado
encontPessoa :: String -> [String]
encontPessoa a = map fst (filter ((== a).snd) emprestimos)


--Função pertence, que verifica se um livro pertence a lista de empréstimos, utilizando polimorfismo
pertence :: Eq x=> x -> [(x,x)] -> Bool
pertence n [] = False
pertence n (x:xs)
 |n == snd x = True
 |otherwise = pertence n xs

-- Função livFoiEmp, que dado um livro, utilizando a função pertence, verifica se ele está emprestado, utilizando map e filter
livFoiEmp :: String -> String
livFoiEmp x 
 |pertence x emprestimos = "Livro esta sendo emprestado"
 |otherwise = "Livro nao esta sendo emprestado"
 
 
-- Função qntdLivEmprPess, que dado uma pessoa, devolve a quantidade de livro que essa pessoa pegou emprestado
qntdLivEmprPess :: String -> Int
qntdLivEmprPess a = length (encontLivro a)

