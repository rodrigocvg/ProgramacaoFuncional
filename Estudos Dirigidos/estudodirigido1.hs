--Programador: Rodrigo Castro
--Data: 27/08/2021

--Declaração dos tipos.
type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia,Mes,Ano)

--Função que devolve se um ano é bissexto ou não.
bissexto :: Ano -> Bool
bissexto ano
 |mod ano 4 ==0 && (mod ano 100 /=0 || mod ano 400 ==0) = True
 |otherwise = False

-- Função que devolve numero de dias em cada mes de um ano.
numDeDiasEmCadaMesDeUmAno :: Ano -> [Int]
numDeDiasEmCadaMesDeUmAno ano = [31,fev,31,30,31,30,31,31,30,31,30,31]
 where 
 fev
  |bissexto ano = 29
  |otherwise = 28
  
--Função que fornecida uma data, fornece o número de dias desde 31 de Dezembro de 2000.
numDeDias :: Data -> Int
numDeDias (dia,mes,ano) =
 dia -- dias deste mes
 + sum (take (mes-1) (numDeDiasEmCadaMesDeUmAno ano))
 + (ano-2001)*365 + (ano-2001)`div`4

-- Função que devolve o nome do dia da semana.
nomeDoDia :: Int -> String
nomeDoDia dia
 |dia==0 = "Domingo"
 |dia==1 = "Segunda"
 |dia==2 = "Terca"
 |dia==3 = "Quarta"
 |dia==4 = "Quinta"
 |dia==5 = "Sexta"
 |dia==6 = "Sabado"

-- Função que forncida uma data, devolve o nome do dia respectivo da semana
diaDaSemana :: Data -> String
diaDaSemana (dia,mes,ano)
 |(dia,mes,ano) == (30,12,2000) = nomeDoDia 0
 |otherwise = nomeDoDia (mod (numDeDias (dia,mes,ano)) (7))

  