{-| 
 Module: Main
 Descrption: funções que permitem o avanço do jogo
 Copyright: Ana Ribeiro
            Jéssica Lemos -}

module Main where

import Data.Char 
import System.Environment
import Text.Read
import Data.Maybe
import Data.List
    
{-| Esta função dado o estado actual do jogo e o número instantes de tempo que faltam para o jogo terminar, devolve o novo estado do jogo. No caso de existirem bombas prestes
a explodir, recorremos à função "juntaAlteraFinal" eliminando a informação das bombas que explodiram. Se existirem bombas prestes a explodir e outras,então, reduzimos o tempo 
das bombas que não estão prestes a explodir ao mapa já com as alterações das bombas que explodiram. Na eventualidade, de existirem bombas com tempo zero, então, alteramo-lo 
para um. Na hipótese de não existir bombas no mapa, é devolvido o mesmo estado de jogo. Nos restantes casos diminuimos ao tempo de explosão das bombas. -}
avanca :: [String] -> Int -> [String]
avanca l n | ((existeBomb l n) == False) = l
           | elem True (haBomb0 l n) = (takeWhile (/= (head(reverse(infBombas l n)))) l) ++ (haBomb0Exp l n) ++ reverse (takeWhile (/= (head(infBombas l n))) (reverse l))
           | (tocaBombaT l n)==True && (haBombExp l n) == True && (existeBNExp l n)== True = (takeWhile (/= (head(reverse(infBombas t n)))) t) ++ (dimTempo t n) ++ (takeWhile (/= (head(infBombas t n))) (reverse t))
           | (haBombExp l n) == True && (existeBNExp l n)== True = (takeWhile (/= (head(reverse(infBombas t n)))) t) ++ (dimTempo t n) ++ reverse((takeWhile (/= (head(infBombas t n))) (reverse t)))
           | (haBombExp l n) == True = (juntaAlteraFinal l n) \\ (infListBomb l n)
           | otherwise = (takeWhile (/= (head((reverse(infBombas l n))))) l) ++ (dimTempo l n) ++ reverse (takeWhile (/= (head(infBombas l n))) (reverse l))
               where t = ((juntaAlteraFinal l n) \\ (infListBomb l n))

{-| Esta função permite a diminuição do tempo nas bombas à medida que a função "avanca" é chamada. Caso uma das bombas tenha tempo de ação 1 então o seu tempo mantém-se. -}
dimTempo :: [String] -> Int -> [[Char]]
dimTempo l n = map f (reverse(infBombas l n))
                    where f h | (reverse(fst(me h l n)))=="1" = h
                              | otherwise = reverse (snd (me h l n)) ++ show ((read (reverse(fst(me h l n))) :: Int) -1)

{-| Esta função devolve um tuplo em que o primeiro elemento corresponde ao tempo de explosão da bomba e o segundo à sua restante informação.-}                              
me :: [Char] -> [String] -> Int -> ([Char], [Char])                          
me h l n = break (==' ') (reverse h)

{-| A "dim" devolve a dimensão do mapa. -}
dim :: [String] -> Int -> Int
dim l n = length ( l !! 0)

{-| Esta função percorre a lista, através de um map aplicado a "infBombas", caso o tempo de ação da bomba seja 0 então este tempo é substituído por 1. -}
haBomb0Exp :: [String] -> Int -> [String]
haBomb0Exp l n = map f (reverse(infBombas l n))
                    where f h | (reverse(fst(me h l n)))=="0" = reverse (drop 1 (reverse h)) ++ "1"
                              | otherwise = h

{-| A função verifica se existem bombas com tempo de explosão 0. Deste modo, percorremos a lista procurando se existe o símbolo da bomba e se o tempo de explosão é 0. -}
haBomb0 :: [String] -> Int -> [Bool]
haBomb0 l n = map f (infBombas l n)
                    where f h | (reverse(fst(me h l n)))=="0" = True
                              | otherwise = False

{-| A "haBombExp" verifica se existe bombas prestes a explodir no mapa.Para tal, percorre a lista comçando pelo fim para ver se existe uma bomba cujo o tempo de explosão 
é um.  -}
haBombExp :: [String] -> Int -> Bool
haBombExp [] n = False
haBombExp l n = if (reverse l) !! 0 !! 0 == '*' && (reverse ((reverse l) !! 0)) !! 0 == '1'
                   then True
                   else haBombExp (drop 1 (reverse l)) n

{-| Esta função verifica se no mapa existem bombas, assim percorremos este verificando se existe o símbolo de bomba. -}
existeBomb :: [String] -> Int -> Bool
existeBomb [] n = False 
existeBomb l n = if (reverse l) !! 0 !! 0 == '*'
                 then True
                 else existeBomb (drop 1 (reverse l)) n 

{-| Com esta função verificamos se existem bombas sem estarem prestes a explodir. Para tal percorremos a lista verificando se existe o símbolo de bomba e se o seu tempo de
explosão é diferente de 1. -}
existeBNExp :: [String] -> Int -> Bool
existeBNExp [] n = False
existeBNExp l n = if (reverse l) !! 0 !! 0 == '*' && (reverse ((reverse l) !! 0)) !! 0 /= '1'
                   then True
                   else existeBNExp (drop 1 (reverse l)) n

{-| A "infBombas" devolve uma lista com todas as bombas do mapa. Assim, percorremos a lista até encontrarmos o símbolo de bomba, caso encontremos guardamos a sua informação 
fazendo recursiva desta para verificarmos se existem mais bombas. -}                
infBombas :: [String] -> Int -> [String] 
infBombas [] n = []
infBombas l n = if (reverse l) !! 0 !! 0 == '*'
                 then [((reverse l) !! 0)] ++ infBombas (drop 1 (reverse l)) n
                 else infBombas (drop 1 (reverse l)) n

{-| A função raio devolve uma lista com os raios das bombas prestes a explodir (ou seja, com tempo igual a 1). Começamos por percorrer a lista até encontrar uma bomba ('*')
e com tempo de explosão 1. Quando esta é encontrada vamos retirar-lhe a informação do seu raio e fazemos uma recursiva para verificar a existência de mais bombas nesta 
situação. Para tal, usamos a função break que permitiu separar o tempo de explosão da bomba da sua restante informação. E de seguida, pegamos no segundo elemento do tuplo 
ao qual retiramos o primeiro elemento visto que correspondia a um ' ' (contém a restante informação da bomba) e recolhemos o primeiro elemento do resultado da função break
para obtermos o raio da bomba. Caso não exista nenhuma bombas prestes a explodir devolve uma lista com raio 0.  -}
raio :: [String] -> Int -> [Int]
raio [] n = []
raio l n = if (reverse l) !! 0 !! 0 == '*' && (reverse ((reverse l) !! 0)) !! 0 == '1'
             then [(read (reverse (fst ((break (==' ') (drop 1 (snd (break (==' ') (reverse ((reverse l) !! 0))))))))) :: Int)] ++ raio (drop 1 (reverse l)) n
             else raio (drop 1 (reverse l)) n

{-| Esta função dá uma lista com a informação de todas as bombas prestes a explodir sem o símbolo da bomba e o espaço entre este e o próximo algarismo. Inicialmente 
percorremos a lista até encontrar uma bomba nestas condições. Quando tal se sucede, retiramos através de "drop 2" o símbolo e o espaço referidos anteriormente à 
informação da bomba. Fazemos de seguida a recursiva para aplicar o mesmo processo às restantes bombas nestas condições.  -}
infBomb :: [String] -> Int -> [String]
infBomb [] n = []
infBomb l n = if (reverse l) !! 0 !! 0 == '*' && (reverse ((reverse l) !! 0)) !! 0 == '1'
                 then [drop 2 ((reverse l) !! 0)] ++ infBomb (drop 1 (reverse l)) n
                 else infBomb (drop 1 (reverse l)) n

{-| A "cooryBomb" fornece uma lista com todas as coordenadas y de todas as bombas prestes a explodir. Assim, aplicamos a função map à "infBomb". Esta percorre todos os 
elementos da lista e aplica-lhes a função f, que isola o primeiro elemento resultante do break. -}
cooryBomb :: [String] -> Int -> [Int]
cooryBomb l n = map f (infBomb l n)
                     where f h = read (fst (break (==' ') h))  :: Int

{-| A "coorxBomb" fornece uma lista com todas as coordenadas x de todas as bombas prestes a explodir. Assim, aplicamos a função map à "infBomb". Esta percorre todos os 
elementos da lista e aplica-lhes a função f, que isola o primeiro elemento do tuplo resultante do break aplicado à remoção da coordenada y, através de um break, e do 
espaço que os separava, recorrendo ao drop. -}
coorxBomb :: [String] -> Int -> [Int]
coorxBomb l n = map f (infBomb l n)
                     where f h = read (fst (break (==' ') (drop 1 (snd (break (==' ') h)))))  :: Int

{-| A função multiplica todas as coordenadas x, das bombas prontas a explodir, pela dimensão do mapa. A função "dim" indica a dimensão do mapa recebido. Para tal 
calculamos o tamanho da cabeça da lista, que corresponde à primeira linha do mapa constituída apenas por pedras. -}
convert :: [String] -> Int -> [Int]
convert l n = map f (coorxBomb l n)
                 where f h = (dim l n) * h

{-| Esta converte as coordenadas das bombas prestes a explodir num número do mapa. Para tal, recorremos à função auxiliar "aux" que junta cada coordenada y mais 1 com a 
correspondente coordenada x, que foi anteriormente multiplicada pela dimensão do mapa, na função "convert". Note-se que conseideramos cada ponto do mapa um número sendo
que o primeiro ponto do mapa corresponde ao numero 1 e assim sucessivamente. -}
convertToNum :: [String] -> Int -> [Int]
convertToNum l n = aux (cooryBomb l n) (convert l n)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (x+y+1):aux xs ys


{-| Esta função devolve uma lista com toda a informação dos jogadores. Assim, percorremos a lista do mapa invertido até encontrarmos um algarismo. que corresponde ao 
identificador de um jogador. Fazendo recursividade verificamos se existiam mais jogadores, caso existam guardamos também a sua informação. -}
infJogs :: [String] -> Int -> [String]
infJogs [] n = []
infJogs l n = if ((head (head (take 1 (reverse l)))>= '0' && (head (head (take 1 (reverse l)))<= '9')))
                 then [drop 2 ((take 1 (reverse l)) !! 0)] ++ infJogs (reverse (drop 1 (reverse l))) n
                 else infJogs (reverse (drop 1 (reverse l))) n

{-| "coorJog" devolve uma lista com a informação dos jogadores sem os Power ups.-}
coorJog :: [String] -> Int -> [String]
coorJog l n = map f (infJogs l n)
                   where f h = h \\ (concat (replicate (length h) "+!" ))

{-| A função dá uma lista de tuplos com as coordenadas x e y dos jogadores. Utilizando um map, aplicamos um break à função que devolvia a informação dos jogadores de
modo a obter as suas coordenadas.  -}
tuploCoordJogs :: [String] -> Int -> [([Char], [Char])]
tuploCoordJogs l n = map f (coorJog l n)
                     where f h = break (==' ') h 

{-| A função dá uma lista com as coordenadas y dos jogadores, para tal convertemos todos os primeiros elementos dos tuplos em algarismos. -}
cooryJogs :: [String] -> Int -> [Int]
cooryJogs l n = map f (tuploCoordJogs l n) 
                where f h = read (fst h) :: Int

{-| A função dá uma lista com as coordenadas x dos jogadores, para tal convertemos todos os segundos elementos dos tuplos em algarismos. -}
coorxJogs :: [String] -> Int -> [Int]
coorxJogs l n = map f (tuploCoordJogs l n) 
                where f h = read (snd h) :: Int

{-| A função multiplica todas as coordenadas x, dos jogadores, pela dimensão do mapa. A função "dim" indica a dimensão do mapa recebido. Para tal 
calculamos o tamanho da cabeça da lista, que corresponde à primeira linha do mapa constituída apenas por pedras. -}
convertJogs :: [String] -> Int -> [Int]
convertJogs l n = map f (coorxJogs l n)
                 where f h = (dim l n) * h

{-| Esta converte as coordenadas dos jogadores num número do mapa. Para tal, recorremos à função auxiliar "aux" que junta cada coordenada y mais 1 com a 
correspondente coordenada x, que foi anteriormente multiplicada pela dimensão do mapa, na função "convert". -}
convertToNumJogs :: [String] -> Int -> [Int]
convertToNumJogs l n = aux (cooryJogs l n) (convertJogs l n)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (x+y+1):aux xs ys

{-| Esta função devolve uma lista com todos os PowerUps do mapa. Assim, percorremos a lista do mapa invertido até encontrarmos o símbolo '+' ou '!' que corresponde a 
um PowerUp. Fazendo recursividade verificamos se existiam mais PowerUps, caso existam guardamos também a sua informação. -}
infPU :: [String] -> Int -> [String]
infPU [] n = []
infPU l n = if ((head (head (take 1 (reverse l)))== '+' || (head (head (take 1 (reverse l)))== '!')))
                 then [drop 2 ((take 1 (reverse l)) !! 0)] ++ infPU (reverse (drop 1 (reverse l))) n
                 else infPU (reverse (drop 1 (reverse l))) n

{-| A função dá uma lista de tuplos com as coordenadas x e y dos PowerUps. Utilizando um map, aplicamos um break à função que devolvia a informação dos PowerUps de
modo a obter as suas coordenadas.-}
tuploCoord :: [String] -> Int -> [([Char], [Char])]
tuploCoord l n = map f (infPU l n)
                  where f h = break (==' ') h 

{-| A função dá uma lista com as coordenadas y dos PowerUps, para tal convertemos todos os primeiros elementos dos tuplos em algarismos. -}
cooryPU :: [String] -> Int -> [Int]
cooryPU l n = map f (tuploCoord l n) 
                where f h = read (fst h) :: Int

{-| A função dá uma lista com as coordenadas x dos PowerUps, para tal convertemos todos os segundos elementos dos tuplos em algarismos.  -}
coorxPU :: [String] -> Int -> [Int]
coorxPU l n = map f (tuploCoord l n) 
                where f h = read (snd h) :: Int

{-| A função multiplica todas as coordenadas x, dos PowerUps, pela dimensão do mapa. A função "dim" indica a dimensão do mapa recebido. Para tal 
calculamos o tamanho da cabeça da lista, que corresponde à primeira linha do mapa constituída apenas por pedras. -}
convertPU :: [String] -> Int -> [Int]
convertPU l n = map f (coorxPU l n)
                 where f h = (dim l n) * h
                       
{-| Esta converte as coordenadas dos PowerUps num número do mapa. Para tal, recorremos à função auxiliar "aux" que junta cada coordenada y mais 1 com a 
correspondente coordenada x, que foi anteriormente multiplicada pela dimensão do mapa, na função "convert". Note-se que conseideramos cada ponto do 
mapa um número sendo que o primeiro ponto do mapa corresponde ao numero 1 e assim sucessivamente. -}
convertToNumPU :: [String] -> Int -> [Int]
convertToNumPU l n = aux (cooryPU l n) (convertPU l n)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (x+y+1):aux xs ys

{-| Esta função dá uma lista com a informação de todas as bombas sem o símbolo da bomba e o espaço entre este e o próximo algarismo. Inicialmente percorremos
a lista até encontrar uma bomba tal como na função "infBomb" sendo que nesta não existe restrição ao tempo que falta para a bomba explodir.-}
infTodasBomb :: [String] -> Int -> [String]
infTodasBomb [] n = []
infTodasBomb l n = if (reverse l) !! 0 !! 0 == '*'
                 then [drop 2 ((reverse l) !! 0)] ++ infTodasBomb (drop 1 (reverse l)) n
                 else infTodasBomb (drop 1 (reverse l)) n
                 
{-| A "cooryTodasBomb" segue o mesmo procedimento da função "cooryBomb". -}
cooryTodasBomb :: [String] -> Int -> [Int]
cooryTodasBomb l n = map f (infTodasBomb l n)
                     where f h = read (fst (break (==' ') h))  :: Int

{-| A "coorxTodasBomb" segue o mesmo raciocínio da função "coorxBomb". -}
coorxTodasBomb :: [String] -> Int -> [Int]
coorxTodasBomb l n = map f (infTodasBomb l n)
                     where f h = read (fst (break (==' ') (drop 1 (snd (break (==' ') h)))))  :: Int


{-| Esta também tem o mesmo procedimento da função "convert". -}
convertTodas :: [String] -> Int -> [Int]
convertTodas l n = map f (coorxTodasBomb l n)
                 where f h = (dim l n) * h

{-| Esta converte as coordenadas das bombas num número do mapa, tendo em conta também a função "convertToNum" -}
convertToNumTodas :: [String] -> Int -> [Int]
convertToNumTodas l n = aux (cooryTodasBomb l n) (convertTodas l n)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (x+y+1):aux xs ys                 

{-| A função "indicaR" devolve uma lista com o número máximo que a chama da bomba pode alcançar para a direita. Para tal, recorremos a uma auxiliar que recebia como 
argumentos a lista com os raios das bombas prestes a explodir e a lista com os números do mapa correspondentes à localização destas bombas. Esta tem como objetivo 
somar ao número do mapa onde se encontra a bomba ao raio correspondente. -}
indicaR :: [String] -> Int -> [Int]
indicaR l n = aux (raio l n) (convertToNum l n)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (x+y):aux xs ys    

{-| Esta função, como o próprio nome indica restringe a informação dada na lista resultante da função "indicaR". Uma vez que a dimensão máxima da chama é o limite 
máximo do mapa à direita, caso o número do mapa correspondente à chama da bomba pertença à linha seguinte, ou seja, maior que -}
restricaoR :: [String] -> Int -> [Int]
restricaoR l n = aux (indicaR l n) (coorxBomb l n)
                   where aux [] [] = []
                         aux [] l = l
                         aux l [] = l
                         aux (x:xs) (y:ys)  | x>(y +1)*(dim l n) = (y+1)*(dim l n) : aux xs ys 
                                            | otherwise = x : aux xs ys



{-| A função "indicaL" devolve uma lista com o número máximo que a chama da bomba pode alcançar pode alcançar para a esquerda. Para tal, recorremos a uma auxiliar 
que recebia como argumentos a lista com os raios das bombas prestes a explodir e a lista com os números do mapa correspondentes à localização destas bombas. Esta 
tem como objetivo subtrair ao número do mapa onde se encontra a bomba ao raio correspondente. -}
indicaL :: [String] -> Int -> [Int]
indicaL l n = aux (raio l n) (convertToNum l n)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (y-x):aux xs ys  

{-| Esta função, como o próprio nome indica restringe a informação dada na lista resultante da função "indicaL". -}
restricaoL :: [String] -> Int -> [Int]
restricaoL l n = aux (indicaL l n) (coorxBomb l n)
                    where aux [] [] = []
                          aux [] l = l
                          aux l [] = l
                          aux (x:xs) (y:ys)  | x<((y* (dim l n)) +1) = (y*(dim l n) +1) : aux xs ys 
                                             | otherwise = x : aux xs ys 

{-| A função "indicaU" devolve uma lista com o número máximo que a chama da bomba pode alcançar pode alcançar para baixo. Para tal, recorremos a uma auxiliar que 
recebia como argumentos a lista com os raios das bombas prestes a explodir e a lista com os números do mapa correspondentes à localização destas bombas. Esta tem
como objetivo subtrair ao número do mapa onde se encontra a bomba o resultado do produto da dimensão com o raio correspondente. -}
indicaU :: [String] -> Int -> [Int]
indicaU l n = aux (raio l n) (convertToNum l n)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (y-(dim l n)*x):aux xs ys

{-| Esta função, como o próprio nome indica restringe a informação dada na lista resultante da função "indicaU". -}
restricaoU :: [String] -> Int -> [Int]
restricaoU l n = aux (indicaU l n) (cooryBomb l n)
                    where aux [] [] = []
                          aux [] l = l
                          aux l [] = l
                          aux (x:xs) (y:ys)  | x<1 = (y+1) : aux xs ys 
                                             | otherwise = x : aux xs ys

{-| A função "indicaD" devolve uma lista com o número máximo que a chama da bomba pode alcançar pode alcançar para a cima. Para tal, recorremos a uma auxiliar que 
recebia como argumentos a lista com os raios das bombas prestes a explodir e a lista com os números do mapa correspondentes à localização destas bombas. Esta tem 
como objetivo somar ao número do mapa onde se encontra a bomba o resultado do produto da dimensão com o raio correspondente -}
indicaD :: [String] -> Int -> [Int]
indicaD l n = aux (raio l n) (convertToNum l n)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (y+(dim l n)*x):aux xs ys

{-| Esta função, como o próprio nome indica restringe a informação dada na lista resultante da função "indicaL". -}
restricaoD :: [String] -> Int -> [Int]
restricaoD l n = aux (indicaD l n) (cooryBomb l n)
                    where aux [] [] = []
                          aux [] l = l
                          aux l [] = l
                          aux (x:xs) (y:ys) | x>((dim l n)^2) = (((dim l n)^2)-((dim l n)-(y+1))) : aux xs ys
                                            | otherwise = x : aux xs ys                       

{-| Esta devolve uma lista de listas com todos os números do mapa pela qual a chama passa. Desta forma, através de uma auxiliar recebemos o número do mapa onde se 
encontram as bombas e o número máximo do mapa até onde a chama pode proceguir, pelo que geramos uma lista cujo primeiro elemento é o número máximo da chama e o último 
é o número da bomba, sendo necessário inverter esta lista uma vez que a chama inicia no local do mapa onde se encontra a bomba. Recorremos à recursiva para as 
restantes bombas prestes a explodir. -}
numerosL :: [String] -> Int -> [[Int]]
numerosL l n = aux (convertToNum l n) (restricaoL l n)
                      where aux [] [] = []
                            aux l [] = [l]
                            aux [] l = [l]
                            aux (x:xs) (y:ys) =  [(reverse [y..(x-1)])] ++ aux xs ys

{-| Esta devolve uma lista de listas com todos os números do mapa pela qual a chama passa. Assim, através de uma auxiliar recebemos o número do mapa onde se 
encontram as bombas e o número máximo do mapa até onde a chama pode proceguir, pelo que geramos uma lista cujo primeiro elemento é o número da bomba mais um e o 
último é o número máxima da chama. Recorremos à recursiva para as restantes bombas prestes a explodir.  -}
numerosR :: [String] -> Int -> [[Int]]
numerosR l n = aux (convertToNum l n) (restricaoR l n)
                      where aux [] [] = []
                            aux l [] = [l]
                            aux [] l = [l]
                            aux (x:xs) (y:ys) = [[(x+1)..y]] ++ aux xs ys


{-| Esta devolve uma lista de listas com todos os números do mapa pela qual a chama passa. Pelo que, através de uma auxiliar recebemos o número do mapa onde se 
encontram as bombas e o número máximo do mapa até onde a chama pode proceguir. Para tal, recorremos a uma lista de compreensão, que cada elemento da lista será 
o número do mapa onde se encontra a bomba menos o produto da dimensão do mapa por um "a" (o que se deve ao facto de a chama se propagar para cima) tal que este "a" vai ser cada elemento de uma lista dos números de um 
até à divisão da diferença dos números do mapa onde se encontra a bomba e da chama máxima pela dimensão deste. É necessária a recursiva para que o mesmo seja 
feito para as restantes bombas prestes a explodir.  -}
numerosU :: [String] -> Int -> [[Int]]
numerosU l n = aux (convertToNum l n) (restricaoU l n)
                      where aux [] [] = []
                            aux l [] = [l]
                            aux [] l = [l]
                            aux (x:xs) (y:ys) = [[x-a*(dim l n) | a <- [1..(div (x-y) (dim l n))]]] ++ aux xs ys

{-| Esta devolve uma lista de listas com todos os números do mapa pela qual a chama passa. Pelo que, através de uma auxiliar recebemos o número do mapa onde se 
encontram as bombas e o número máximo do mapa até onde a chama pode proceguir. Para tal, recorremos a uma lista de compreensão, que cada elemento da lista será 
o número do mapa onde se encontra a bomba mais o produto da dimensão do mapa por um "a" (o que se deve ao facto de a chama se propagar para baixo) tal que este 
"a" vai ser cada elemento de uma lista dos números de um até à divisão da diferença dos números do mapa onde se encontra a bomba e da chama máxima pela dimensão 
deste. É necessária a recursiva para que o mesmo seja feito para as restantes bombas prestes a explodir. -}
numerosD :: [String] -> Int -> [[Int]]
numerosD l n = aux (convertToNum l n) (restricaoD l n)
                      where aux [] [] = []
                            aux l [] = [l]
                            aux [] l = [l]
                            aux (x:xs) (y:ys) = [[x+a*(dim l n) | a <- [1..(div (y-x) (dim l n))]]] ++ aux xs ys

{-| A função transforma a listas devolvida pela funcão "numerosR" nos correspondentes símbolos do mapa: "#"," " e "?". Para tal usamos uma auxiliar que tem com 
argumento a função "numerosR" (se o último elemento uma vez que é uma lista vazia resultante do caso de paragem). Esta chama a função "convertToSimb" que converte
cada número de uma lista no respetivo símbolo. Posteriormente, é feita a recursiva da função "aux" para que sejam convertidas as restantes listas das restantes 
bombas prestes a explodir. -}
convertToSimbR :: [String] -> Int -> [[String]]
convertToSimbR l n = aux ((numerosR l n)) l
                         where aux [] l = []
                               aux (x:xs) l = [convertToSimb x l] ++ aux xs l
                               convertToSimb x l = map f x
                               f h = [head (drop ((mod h (dim l n)) -1) (head (take 1 (drop (div h (dim l n)) l))))]

{-| A função transforma a listas devolvida pela funcão "numerosD" nos correspondentes símbolos do mapa: "#"," " e "?". -}
convertToSimbD :: [String] -> Int -> [[String]]
convertToSimbD l n = aux (numerosD l n) l
                         where aux [] l = []
                               aux (x:xs) l = [convertToSimb x l] ++ aux xs l
                               convertToSimb x l = map f x
                               f h = [head (drop ((mod h (dim l n)) -1) (head (take 1 (drop (div h (dim l n)) l))))]

{-| A função transforma a listas devolvida pela funcão "numerosL" nos correspondentes símbolos do mapa: "#"," " e "?". Esta segue o mesmo procedimento da função 
anterior diferindo apenas o argumento que é a função "numerosL". -}
convertToSimbL :: [String] -> Int -> [[String]]
convertToSimbL l n = aux ((numerosL l n)) l
                         where aux [] l = []
                               aux (x:xs) l = [convertToSimb x l] ++ aux xs l
                               convertToSimb x l = map f x
                               f h = [head (drop ((mod h (dim l n)) -1) (head (take 1 (drop (div h (dim l n)) l))))]

{-| A função transforma a listas devolvida pela funcão "numerosU" nos correspondentes símbolos do mapa: "#"," " e "?". Esta segue o mesmo procedimento da função 
anterior diferindo apenas o argumento.-}
convertToSimbU :: [String] -> Int -> [[String]]
convertToSimbU l n = aux (numerosU l n) l
                         where aux [] l = []
                               aux (x:xs) l = [convertToSimb x l] ++ aux xs l
                               convertToSimb x l = map f x
                               f h = [head (drop ((mod h (dim l n)) -1) (head (take 1 (drop (div h (dim l n)) l))))]

{-| A "toNumerosU" converte a lista devolvida pela "convertToSimbU" através de uma auxiliar que chama a função "toNum" que é aplicada a cada lista das listas 
recebidas, daqui ser feita a recursividade da função auxiliar. -}
toNumerosU :: [String] -> Int -> [[Int]]
toNumerosU l n = aux (convertToSimbU l n) l
                 where aux [] l = []
                       aux (x:xs) l = [toNum x] ++ aux xs l

{-| A "toNumerosL" converte a lista devolvida pela "convertToSimbL" através de uma auxiliar que chama a função "toNum", assim como na função "toNumerosU". -}
toNumerosL :: [String] -> Int -> [[Int]]
toNumerosL l n = aux (convertToSimbL l n) l
                 where aux [] l = []
                       aux (x:xs) l = [toNum x] ++ aux xs l

{-| A "toNumerosD" converte a lista devolvida pela "convertToSimbD" através de uma auxiliar que chama a função "toNum", tal como na função anterior. -}
toNumerosD :: [String] -> Int -> [[Int]]
toNumerosD l n = aux (convertToSimbD l n) l
                 where aux [] l = []
                       aux (x:xs) l = [toNum x] ++ aux xs l

{-| A "toNumerosR" converte a lista devolvida pela "convertToSimbR" através de uma auxiliar que chama a função "toNum", tal como na função anterior. -}
toNumerosR :: [String] -> Int -> [[Int]]
toNumerosR l n = aux (convertToSimbR l n) l
                 where aux [] l = []
                       aux (x:xs) l = [toNum x] ++ aux xs l

{-| A função percorre uma lista convertendo os seus elementos num determinado número. Caso seja "#" (pedra) corresponde ao 0, se for "?" (tijolo) é 1 e se for
" " (vazio) é 2. -}
toNum :: [String] -> [Int]
toNum x = map f x
             where f h | h == "#" = 0
                       | h == "?" = 1
                       | h == " " = 2

{-| Esta função recorre à função "verificaAux" que tem como argumentos a função "toNumerosR" e "numerosR". Sendo o seu resultado uma lista com tuplos em que o 
primeiro elemento corresponde ao número do mapa onde haverá uma alteração e o segundo elemento é a alteração que irá estar associada. -}
verificaR :: [String] -> Int -> [(Int,Int)]
verificaR l n = verificaAux (toNumerosR l n) (numerosR l n) l n


{-| Esta função recorre à função "verificaAux" que tem como argumentos a função "toNumerosR" e "numerosR". -}
verificaU :: [String] -> Int -> [(Int,Int)]
verificaU l n = verificaAux (toNumerosU l n) (numerosU l n) l n

{-| Esta função recorre à função "verificaAux" que tem como argumentos a função "toNumerosR" e "numerosR". -}
verificaD :: [String] -> Int -> [(Int,Int)]
verificaD l n = verificaAux (toNumerosD l n) (numerosD l n) l n

{-| Esta função recorre à função "verificaAux" que tem como argumentos a função "toNumerosR" e "numerosR". -}
verificaL :: [String] -> Int -> [(Int,Int)]
verificaL l n = verificaAux (toNumerosL l n) (numerosL l n) l n

{-| A função "verificaAux" devolve uma lista de tuplos. Se verificar-se que a função "ve" tem como resutado (0,0) então corresponde a um local do mapa que é apenas
vazio pelo que a chama prossegue para o ponto seguinte logo é chamada recursivamente a função sem o primeiro elemento das respetivas listas. Se a "ve" corresponder
a um jogador, armazenamos a sua informação dada pelo seu tuplo e repetimos o processo anterior. O mesmo se sucede se a "ve" for uma bomba. Caso não seja nenhum destes 
casos então guarda-se a informação proveniente da função "ve" numa lista e, mais uma vez, faz-se a recursiva.  -}      
verificaAux :: [[Int]] -> [[Int]] -> [String] -> Int -> [(Int,Int)]           
verificaAux [] [] l n = []
verificaAux l1 [] l n = []
verificaAux [] l2 l n = []
verificaAux (x:xs) (y:ys) l n | (ve x y l n)== (0,0) = verificaAux (((delete (x!!0) x):xs)\\[[]]) (((delete (y!!0) y):ys)\\[[]]) l n                  
                              | (ve x y l n)== ((y!!0),4) = [((y!!0),4)] ++ verificaAux (((delete (x!!0) x):xs)\\ [[]]) (((delete (y!!0) y):ys)\\[[]]) l n 
                              | (ve x y l n)== ((y!!0),5) = [((y!!0),5)] ++ verificaAux (((delete (x!!0) x):xs)\\[[]]) (((delete (y!!0) y):ys)\\[[]]) l n
                              | otherwise = [ve x y l n] ++ verificaAux xs ys l n 

{-| A "confirmaJog" verifica se existe algum jogador no local onde foi colocada uma bomba. Assim, verificamos se algum dos elementos da lista com os números do 
mapa onde se encontram as bombas prestes a explodir se repete na lista com os números do mapa onde estão os jogadores. -}
confirmaJog :: [String] -> Int -> Bool
confirmaJog l n = aux (convertToNum l n) (convertToNumJogs l n)      
                    where aux [] l = False
                          aux (x:xs) l = if elem x l then True else aux xs l 

{-| Esta função indica o local onde se encontra o jogador que corresponde ao lugar da bomba.Para tal, verificamos se algum dos elementos da lista com os locais 
das bombas prestes a explodir se encontra também na lista com o número do mapa onde se estão os jogadores. -}
lugarJog :: [String] -> Int -> Int 
lugarJog l n = aux (convertToNum l n) (convertToNumJogs l n)      
                   where aux (x:xs) l = if elem x l then x else aux xs l

{-| Esta função devolve o tuplo associado a um determinado local. Caso o primeiro elemento da lista recebida seja um 0 então é devolvido um tuplo cujo primeiro 
elemento é o local do mapa associado a este, ou seja, o primeiro elemento de y e o segundo elemento é 0. Se for 1 então verifica-se a mesma situação diferindo 
apenas nos valores. Caso seja 2 e o número do mapa corresponda a um local com um PowerUp então é devolvido um tuplo cujo primeiro elemento é o número do mapa e 
o segundo é 3. Se o local da bomba corresponder ao número do mapa onde se encontra um jogador então é devolvido um tuplo cujo primeiro elemento é o número do 
mapa e o segundo é o número 4. Se for 2 e o número do mapa corresponder a um local com um jogador é devolvido um tuplo com o primeiro elemento igual ao número 
do mapa e o segundo é 4. Caso seja 2 e nesse local do mapa haja uma bomba então é devolvido um tuplo com o primeiro elemento igual ao número do mapa e o segundo 
é 5. Senão é devolvido o tuplo (0,0). -}
ve :: [Int] -> [Int] -> [String] -> Int -> (Int,Int)
ve x y l n | (x!!0)==0 = ((y!!0),(x!!0))     --caso de ser uma pedra
           | (x!!0)==1 = ((y!!0),(x!!0))    --caso de ser tijolo
           | (x!!0)==2 && elem (y!!0) (convertToNumPU l n) = ((y!!0),3)    --caso de ter um PowerUp
           | (x!!0)==2 && elem (y!!0) (convertToNumJogs l n) = ((y!!0),4)  --caso de ter um jogador
           | (x!!0)==2 && elem (y!!0) (convertToNumTodas l n) = ((y!!0),5)  --caso de ter uma bomba
           | otherwise = (0,0)                                              --caso de ser vazia 

{-| Esta função verifica se a explosão de uma bomba causa a explosão de outra, à sua esquerda. -}
tocaBombaL :: [String] -> Int -> Bool
tocaBombaL l n = aux (toNumerosL l n) (numerosL l n) 
                   where aux [] [] = False
                         aux (x:xs) (y:ys) = if ve x y l n == ((y!!0),5)
                                               then True
                                               else aux xs ys

{-| Esta função verifica se a explosão de uma bomba causa a explosão de outra, à sua direita. -}
tocaBombaR :: [String] -> Int -> Bool
tocaBombaR l n = aux (toNumerosR l n) (numerosR l n) 
                   where aux [] [] = False
                         aux (x:xs) (y:ys) = if ve x y l n == ((y!!0),5)
                                               then True
                                               else aux xs ys

{-| Esta função verifica se a explosão de uma bomba causa a explosão de outra, a cima de si. -}
tocaBombaU :: [String] -> Int -> Bool
tocaBombaU l n = aux (toNumerosU l n) (numerosU l n) 
                   where aux [] [] = False
                         aux (x:xs) (y:ys) = if ve x y l n == ((y!!0),5)
                                               then True
                                               else aux xs ys
{-| Esta função verifica se a explosão de uma bomba causa a explosão de outra, a baixo de si. -}
tocaBombaD :: [String] -> Int -> Bool
tocaBombaD l n = aux (toNumerosD l n) (numerosD l n) 
                   where aux [] [] = False
                         aux (x:xs) (y:ys) = if ve x y l n == ((y!!0),5)
                                               then True
                                               else aux xs ys

{-| Esta função verifica se a explosão de uma bomba causa a explosão de outra. -}
tocaBombaT :: [String] -> Int -> Bool
tocaBombaT l n = ((tocaBombaL l n)==True) || ((tocaBombaD l n)==True) || ((tocaBombaR l n)==True) || ((tocaBombaU l n)==True)

{-| A função devolve a coordenada x correspondente ao número do mapa que é o primeiro elemento do tuplo. Para tal dividimos o número pela dimensão do mapa. -}         
coX ::(Int,Int) -> [String] -> Int -> Int
coX h l n = (div (fst h) (dim l n))

{-| A função devolve a coordenada Y correspondente ao número do mapa que é o primeiro elemento do tuplo. Logo, ao resto da divisão do número pela dimensão do 
  mapa subtraímos um. -} 
coY ::(Int,Int) -> [String] -> Int -> Int
coY h l n = ((mod (fst h) (dim l n)) -1)

{-| Esta função devolve o mapa com as alterações causadas à direita pela explosão da bomba. Para tal percorremos a lista devolvida pela "verificaR" realizando 
as alterações através da função "altera". -}
juntaAlteraR :: [String] -> Int -> [String]
juntaAlteraR l n = altera (verificaR l n) (altera (drop 1 (verificaR l n)) l n) n  

{-| Esta função devolve o mapa com as alterações causadas à direita e à esquerda pela explosão da bomba. Para tal percorremos a lista devolvida pela "verificaL" 
realizando as alterações através da função "altera". Sendo que neste caso o mapa inicial é o mapa devolvido pela função "juntaAlteraR".-}
juntaAlteraMaisL :: [String] -> Int -> [String]
juntaAlteraMaisL l n = altera (verificaL l n) (altera (drop 1 (verificaL l n)) (juntaAlteraR l n) n) n 

{-| Esta função devolve o mapa com as alterações causadas à direita, à esquerda e para baixo pela explosão da bomba. Para tal percorremos a lista devolvida pela 
"verificaD" realizando as alterações através da função "altera". Sendo que neste caso o mapa inicial é o mapa devolvido pela função "juntaAlteraMaisL".-}
juntaAlteraMaisD :: [String] -> Int -> [String]
juntaAlteraMaisD l n = altera (verificaD l n) (altera (drop 1 (verificaD l n)) (juntaAlteraMaisL l n) n) n 

{-| Esta função devolve o mapa com as alterações causadas, para todos os lados, pela explosão da bomba. Para tal percorremos a lista devolvida pela 
"verificaU" realizando as alterações através da função "altera". Sendo que neste caso o mapa inicial é o mapa devolvido pela função "juntaAlteraMaisD".-}
juntaAlteraFinal :: [String] -> Int -> [String]
juntaAlteraFinal l n | ((confirmaJog l n )== True) = (altera (verificaU l n) (altera (drop 1 (verificaU l n)) (juntaAlteraMaisD l n) n) n) \\ (infMorreJog ((lugarJog l n),4) l n)
                     | otherwise = altera (verificaU l n) (altera (drop 1 (verificaU l n)) (juntaAlteraMaisD l n) n) n

{-| A "altera" devolve o mapa com as alterações realizadas pela explosão de bombas. Caso a chama encontre uma pedra, o mapa devolvido é o inicial. Se encontrar 
um tijolo (1) este terá de ser eliminado do mapa. Quando encontra um PowerUp (3) a informação deste tem de ser eliminada. Se encontrar um jogador (4) este irá morrer, 
logo a sua informação será eliminada. Quando encontra uma bomba (5) esta irá explodir e como tal o seu tempo de ação passa para 1.  -}
altera :: [(Int,Int)] -> [String] -> Int -> [String]
altera [] l n = l
altera (h:t) l n | (snd h == 0) = l
                 | (snd h == 1) = (take (coX h l n) l) ++ [concat ([take (coY h l n) ((take 1 ((drop (coX h l n) l)) !! 0))] ++ [" "] ++ [(drop ((coY h l n)+1) ((take 1 ((drop (coX h l n) l))) !! 0))])] ++ (drop ((coX h l n)+1) l) 
                 | (snd h == 3) = l \\ (deletePU h l n)
                 | (snd h == 4) = l \\ (infMorreJog h l n)
                 | (snd h == 5) = (takeWhile (/= (infExpBomb h l n)) l) ++ [(novaBomb h l n)] ++ (takeWhile (/= (infExpBomb h l n)) (reverse l))

{-| Esta função tem como resultado uma lista com a informação de todos os PowerUps presentes no mapa. Assim, percorremos a lista invertida com a informação 
do mapa até encontrarmos o símbolo dos PowerUps ("!" ou "+"). Quando encontrado é guardada a sua informação e fazemos a recursiva para verificar se existem 
mais PowerUps.-}
infTodosPU :: (Int,Int) -> [String] -> Int -> [String]
infTodosPU h [] n = []
infTodosPU h l n =  if ((head (head (take 1 (reverse l)))== '+' || (head (head (take 1 (reverse l)))== '!')))
                      then (take 1 (reverse l)) ++ infTodosPU h (reverse (drop 1 (reverse l))) n
                      else infTodosPU h (reverse (drop 1 (reverse l))) n

{-| A função devolve a lista com a informação do PowerUp que será atingido pela bomba. Para isso, recorrendo à "infTodosPU" verificamos 
qual destes tem as coordenadas x e y iguais à coordenadas do número do mapa onde irá ocorrer a alteração. -}
deletePU ::(Int,Int) -> [String] -> Int -> [String]
deletePU h l n = aux (infTodosPU h l n)
                where aux [] = [[]]
                      aux (x:xs) = if ((coX h l n)== (digitToInt (last x)) && ((coY h l n) == (digitToInt(x !! 2))))
                                     then [x]
                                     else aux xs

{-| Esta função tem como objetivo uma lista com a informação de todos os jogadores. Assim, percorremos a lista do mapa invertido até encontrarmos um algarismo
que corresponde ao identificador de um jogador. Fazendo recursividade verificamos se existiam mais jogadores, caso existam guardamos também a sua informação. -}
infTodosJog ::(Int,Int) -> [String] -> Int -> [String]
infTodosJog h [] n = [] 
infTodosJog h l n = if ((head (head (take 1 (reverse l)))>= '0' && (head (head (take 1 (reverse l)))<= '9')))
                      then (take 1 (reverse l)) ++ infTodosJog h (reverse (drop 1 (reverse l))) n
                      else infTodosJog h (reverse (drop 1 (reverse l))) n 

{-| "infMorreJog" devolve uma lista com a informação do jogador que irá morrer, devido ao facto de estar no raio de ação da bomba que irá explodir. Assim, recorrendo
à "infTodosJog" verificamos qual destas tem as coordenadas x e y iguais à coordenadas do número do mapa onde irá ocorrer a alteração. -}
infMorreJog :: (Int,Int) -> [String] -> Int -> [String]
infMorreJog h l n = aux (infTodosJog h l n)
                where aux [] = [[]]
                      aux (x:xs) = if ((coX h l n)== (digitToInt (alg x h l n)) && ((coY h l n) == (digitToInt(x !! 2))))
                                     then [x]
                                     else aux xs
{-| Esta função devolve a coordenada x do jogador. -}                                     
alg :: [Char]-> (Int,Int) -> [String] -> Int -> Char                                     
alg x h l n | elem '+' x || elem '!' x =  last (reverse (drop 1 (reverse (x \\ (concat (replicate (length x) "+!" ))))))
            | otherwise = last x

{-| Esta função permite a separação do tempo de explosão da bomba da restante informação, sendo que o segundo elemento corresponde à restante informação. -}
tuplo :: (Int,Int) -> [String] -> Int -> ([Char],[Char]) 
tuplo h l n = break (==' ') (reverse (infExpBomb h l n))

{-| A função altera a informação da bomba que irá ser afetada pela explosão, sendo alterado o seu tempo de explosão, que passará a ser 1. -}
novaBomb :: (Int,Int) -> [String] -> Int -> [Char]
novaBomb h l n = reverse (snd (tuplo h l n)) ++ "1"

{-| "infExpBomb" devolve uma lista com a informação da bomba que irá explodir, devido ao facto de estar no raio de ação de outra bomba que irá explodir. Assim, recorrendo
à "infTodasBomb" verificamos qual destas tem as coordenadas x e y iguais à coordenadas do número do mapa onde irá ocorrer a alteração. Uma vez que, a função à qual recorremos
não contém o símbolo da bomba, quando encontrarmos a bomba nas condições anteriores acrescentamos o símbolo. -}
infExpBomb :: (Int,Int) -> [String] -> Int -> String
infExpBomb h l n = aux (infTodasBomb l n)
                where aux [] = []
                      aux (x:xs) = if ((coX h l n)== (digitToInt (x !! 2)) && ((coY h l n) == (digitToInt(x !! 0))))
                                     then "* "++x
                                     else aux xs

{-| A função "infListBomb" devolve uma lista com a informação das bombas que explodiram. Para tal percorremos a lista do mapa até encontramos o símbolo 
da bomba "*" e chamamos recursivamente a função para verificar se existem mais bombas que irão explodir.  -}
infListBomb :: [String] -> Int -> [String] 
infListBomb [] n = []
infListBomb l n = if (reverse l) !! 0 !! 0 == '*' && (reverse ((reverse l) !! 0)) !! 0 == '1'
                 then [((reverse l) !! 0)] ++ infListBomb (drop 1 (reverse l)) n
                 else infListBomb (drop 1 (reverse l)) n


                 
{-| Função que permite testar esta tarefa -}
main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"
