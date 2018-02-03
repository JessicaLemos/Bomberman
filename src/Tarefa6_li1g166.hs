{-| 
 Module: Main
 Descrption: funções que permitem a implementação da estratégia de jogo do nosso jogador
 Copyright: Ana Ribeiro
            Jéssica Lemos -}

module Tarefa6_li1g166 where
import Data.Char
import Data.List

{-| Esta função devolve os movimentos que o jogador deverá executar. Caso este permaneça no mesmo local e o local para onde pretende ir seja um tijolo, 
então este deverá colocar uma bomba. Na eventualidade, de este se encontrar no canto esquerdo (superior ou inferior) e se encontrar no raio de ação de
uma bomba então este deve movimentar-se para a direita. Se este se encontrar no canto direito (superior ou inferior) e se encontrar no raio de ação de
uma bomba então este deve movimentar-se para a esquerda. No caso de se encontrar acima da fila do meio do mapa e no raio de ação de uma bomba deve exercutar o comando 'U'. Se se encontrar abaixo então movimenta-se baixo. Nos outros casos, o jogador executa os movimentos previstos pela função "just". -}
bot :: [String] -> Int -> Int -> Maybe Char
bot [] p t = Nothing 
bot l p t | ((coorOurJogNum l p t == ((d l p t)^2 -2*(d l p t) +2))||(coorOurJogNum l p t == (d l p t)+2)) && ((bombaJog l p t)==True) = Just 'R'
          | ((coorOurJogNum l p t == ((d l p t)^2 -(d l p t) -1))||(coorOurJogNum l p t == (2*(d l p t))-1)) && ((bombaJog l p t)==True) = Just 'L'
          | (coorOurJogNum l p t) >= ((div ((d l p t)-1) 2)+1)*(d l p t) && ((bombaJog l p t)==True)  = Just 'D' 
          | (coorOurJogNum l p t) <= ((div ((d l p t)-1) 2)+1)*(d l p t) && ((bombaJog l p t)==True)  = Just 'U' 
          | coorOurJogNum l p t == coorOurJogNum l p (t-1) && ((convertS l p t)=='?') = Just 'B'  
          | otherwise = just l p t

{-| Esta função dá uma lista com a informação de todas as bombas prestes a explodir sem o símbolo da bomba e o espaço entre este e o próximo algarismo. Inicialmente 
percorremos a lista até encontrar uma bomba nestas condições. Quando tal se sucede, retiramos através de "drop 2" o símbolo e o espaço referidos anteriormente à 
informação da bomba. Fazemos de seguida a recursiva para aplicar o mesmo processo às restantes bombas nestas condições.  -}
infBomb :: [String] -> Int -> Int -> [String]
infBomb [] p t = []
infBomb l p t = if (reverse l) !! 0 !! 0 == '*' 
                 then [drop 2 ((reverse l) !! 0)] ++ infBomb (drop 1 (reverse l)) p t
                 else infBomb (drop 1 (reverse l)) p t

{-| A "cooryBomb" fornece uma lista com todas as coordenadas y de todas as bombas prestes a explodir. Assim, aplicamos a função map à "infBomb". Esta percorre todos os 
elementos da lista e aplica-lhes a função f, que isola o primeiro elemento resultante do break. -}
cooryBomb :: [String] -> Int -> Int -> [Int]
cooryBomb l p t = map f (infBomb l p t)
                     where f h = read (fst (break (==' ') h))  :: Int

{-| A "coorxBomb" fornece uma lista com todas as coordenadas x de todas as bombas prestes a explodir. Assim, aplicamos a função map à "infBomb". Esta percorre todos os 
elementos da lista e aplica-lhes a função f, que isola o primeiro elemento do tuplo resultante do break aplicado à remoção da coordenada y, através de um break, e do 
espaço que os separava, recorrendo ao drop. -}
coorxBomb :: [String] -> Int -> Int -> [Int]
coorxBomb l p t = map f (infBomb l p t)
                     where f h = read (fst (break (==' ') (drop 1 (snd (break (==' ') h)))))  :: Int

{-| A função multiplica todas as coordenadas x, das bombas prontas a explodir, pela dimensão do mapa. A função "dim" indica a dimensão do mapa recebido. Para tal 
calculamos o tamanho da cabeça da lista, que corresponde à primeira linha do mapa constituída apenas por pedras. -}
convert :: [String] -> Int -> Int -> [Int]
convert l p t = map f (coorxBomb l p t)
                 where f h = (d l p t) * h

{-| Esta converte as coordenadas das bombas prestes a explodir num número do mapa. Para tal, recorremos à função auxiliar "aux" que junta cada coordenada y mais 1 com a 
correspondente coordenada x, que foi anteriormente multiplicada pela dimensão do mapa, na função "convert". Note-se que conseideramos cada ponto do mapa um número sendo
que o primeiro ponto do mapa corresponde ao numero 1 e assim sucessivamente. -}
convertToNum :: [String] -> Int -> Int -> [Int]
convertToNum l p t = aux (cooryBomb l p t) (convert l p t)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (x+y+1):aux xs ys                     
{-| Esta função verifica se o jogador se encontra num local do mapa sob ação de uma bomba. Assim, temos de verificar se o seu número no mapa corresponde a um dos números 
da lista com os números do mapa em que a chama se pode propagar. -}
bombaJog :: [String] -> Int -> Int -> Bool
bombaJog l p t =  elem (coorOurJogNum l p t) (locaisComChama l p t ) 

{-| A função "infOurJog" devolve uma lista com a informação do nosso jogador. Para tal percorremos a informação do mapa até encontrarmos uma lista cujo primeiro elemento
é o dígito igual ao do nosso jogador. -}
infOurJog :: [String] -> Int -> Int -> [String]
infOurJog [] p t = []
infOurJog l p t = if ((head (head (take 1 l)))== intToDigit p)
                      then take 1 l
                      else infOurJog (drop 1 l) p t

{-| A função devolve um tuplo cujo primeiro elemento é o identificador do jogador e o segundo contém as coordenadas do jogador. -}
tuploCoord :: [String] -> Int -> Int -> ([Char],[Char])
tuploCoord l p t = break (==' ') (head(infOurJog l p t))

{-| A "tuploCoord2" devolve um tuplo cujo primeiro elemento contém a coordenada y e o segundo contém a coordenada x. -}
tuploCoord2 :: [String] -> Int -> Int -> ([Char],[Char])
tuploCoord2 l p t = break (==' ') (drop 1 (snd(tuploCoord l p t)))

{-| Esta devolve a coordenada y do nosso jogador. Para tal, é necessário converter o primeiro elemento do tuplo num algarismo. -}
coordy :: [String] -> Int -> Int -> Int
coordy l p t = digitToInt(head(fst(tuploCoord2 l p t)))

{-| Esta devolve a coordenada x do nosso jogador. Para tal, é necessário converter o segundo elemento do tuplo num algarismo. -}
coordx :: [String] -> Int -> Int -> Int
coordx l p t = digitToInt(head(reverse(snd(tuploCoord2 l p t))))


{-| A função "coorOurJogNum" devolve as coordenadas do nosso jogador. -}
coorOurJogNum :: [String] -> Int -> Int -> Int
coorOurJogNum l p t = (d l p t) *(coordx l p t) + ((coordy l p t)+1)

{-| A função devolve o símbolo da célula para a qual o jogador pretende ir. -}
convertS :: [String] -> Int -> Int -> Char
convertS l p t = (head (drop ((mod (mOurjog l p t) (d l p t)) -1) (head (take 1 (drop (div (mOurjog l p t) (d l p t))l)))))

{-| Esta função devolve o o número do mapa associado ao movimento do jogador. Se este pretender movimentar-se para cima então à sua coordenada retiramos a dimensão.
Na possibilidade, de se movimentar para baixo então aumentamos à coordenada a dimensão. Enquanto que para a esquerda diminuímos um e para a esquerda diminuímos um. 
Noutros casos, a sua coordenada mantém-se a mesma.   -}
mOurjog :: [String] -> Int -> Int -> Int
mOurjog l p t | just l p t == Just 'D' = (coorOurJogNum l p t) + (d l p t)
              | just l p t == Just 'U' = (coorOurJogNum l p t) - (d l p t)
              | just l p t == Just 'L' = (coorOurJogNum l p t)-1
              | just l p t == Just 'R' = (coorOurJogNum l p t)+1
              | otherwise = (coorOurJogNum l p t)

{-| Esta função devolve o tamanho do mapa. -}
d :: [String] -> Int -> Int -> Int
d l p t = length ( l !! 0)

{-| Esta função devolve o o número do mapa associado ao movimento do jogador. Se este pretender movimentar-se para cima então à sua coordenada retiramos a dimensão.
Na possibilidade, de se movimentar para baixo então aumentamos à coordenada a dimensão. Enquanto que para a esquerda diminuímos um e para a esquerda diminuímos um. 
Noutros casos, a sua coordenada mantém-se a mesma.   -}
just :: [String] -> Int -> Int -> Maybe Char
just l p t | (coorOurJogNum l p t) <= ((div ((d l p t)-1) 2)+1)*(d l p t) = Just 'D'  
           | (coorOurJogNum l p t) >= ((div ((d l p t)-1) 2)+2)*(d l p t) = Just 'U'  
           | (coorOurJogNum l p t) < ((div ((d l p t)-1) 2)+2)*(d l p t)-(div ((d l p t)-1) 2) = Just 'R'
           | otherwise = Just 'L'

{-| A função "indicaR" devolve uma lista com o número máximo que a chama da bomba pode alcançar para a direita. Para tal, recorremos a uma auxiliar que recebia como 
argumentos a lista com os raios das bombas prestes a explodir e a lista com os números do mapa correspondentes à localização destas bombas. Esta tem como objetivo 
somar ao número do mapa onde se encontra a bomba ao raio correspondente. -}
indicaR :: [String] -> Int -> Int -> [Int]
indicaR l p t = aux (raio l p t) (convertToNum l p t)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (x+y):aux xs ys    

{-| Esta função, como o próprio nome indica restringe a informação dada na lista resultante da função "indicaR". Uma vez que a dimensão máxima da chama é o limite 
máximo do mapa à direita, caso o número do mapa correspondente à chama da bomba pertença à linha seguinte, ou seja, maior que -}
restricaoR :: [String] -> Int -> Int -> [Int]
restricaoR l p t = aux (indicaR l p t) (coorxBomb l p t)
                   where aux [] [] = []
                         aux [] l = l
                         aux l [] = l
                         aux (x:xs) (y:ys)  | x>(y +1)*(d l p t) = (y+1)*(d l p t) : aux xs ys 
                                            | otherwise = x : aux xs ys

{-| A função "indicaU" devolve uma lista com o número máximo que a chama da bomba pode alcançar pode alcançar para baixo. Para tal, recorremos a uma auxiliar que 
recebia como argumentos a lista com os raios das bombas prestes a explodir e a lista com os números do mapa correspondentes à localização destas bombas. Esta tem
como objetivo subtrair ao número do mapa onde se encontra a bomba o resultado do produto da dimensão com o raio correspondente. -}
indicaU :: [String] -> Int -> Int -> [Int]
indicaU l p t = aux (raio l p t) (convertToNum l p t)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (y-(d l p t)*x):aux xs ys

{-| Esta função, como o próprio nome indica restringe a informação dada na lista resultante da função "indicaU". -}
restricaoU :: [String] -> Int -> Int -> [Int]
restricaoU l p t = aux (indicaU l p t) (cooryBomb l p t)
                    where aux [] [] = []
                          aux [] l = l
                          aux l [] = l
                          aux (x:xs) (y:ys)  | x<1 = (y+1) : aux xs ys 
                                             | otherwise = x : aux xs ys       

{-| A função "indicaD" devolve uma lista com o número máximo que a chama da bomba pode alcançar pode alcançar para a cima. Para tal, recorremos a uma auxiliar que 
recebia como argumentos a lista com os raios das bombas prestes a explodir e a lista com os números do mapa correspondentes à localização destas bombas. Esta tem 
como objetivo somar ao número do mapa onde se encontra a bomba o resultado do produto da dimensão com o raio correspondente -}
indicaD :: [String] -> Int -> Int -> [Int]
indicaD l p t = aux (raio l p t) (convertToNum l p t)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (y+(d l p t)*x):aux xs ys

{-| Esta função, como o próprio nome indica restringe a informação dada na lista resultante da função "indicaL". -}
restricaoD :: [String] -> Int -> Int -> [Int]
restricaoD l p t = aux (indicaD l p t) (cooryBomb l p t)
                    where aux [] [] = []
                          aux [] l = l
                          aux l [] = l
                          aux (x:xs) (y:ys) | x>((d l p t)^2) = (((d l p t)^2)-((d l p t)-(y+1))) : aux xs ys
                                            | otherwise = x : aux xs ys   
                                            
{-| A função "indicaL" devolve uma lista com o número máximo que a chama da bomba pode alcançar pode alcançar para a esquerda. Para tal, recorremos a uma auxiliar 
que recebia como argumentos a lista com os raios das bombas prestes a explodir e a lista com os números do mapa correspondentes à localização destas bombas. Esta 
tem como objetivo subtrair ao número do mapa onde se encontra a bomba ao raio correspondente. -}
indicaL :: [String] -> Int -> Int -> [Int]
indicaL l p t = aux (raio l p t) (convertToNum l p t)
                 where aux [] [] = []
                       aux l [] = l
                       aux [] l = l
                       aux (x:xs) (y:ys) = (y-x):aux xs ys  

{-| Esta função, como o próprio nome indica restringe a informação dada na lista resultante da função "indicaL". -}
restricaoL :: [String] -> Int -> Int -> [Int]
restricaoL l p t = aux (indicaL l p t) (coorxBomb l p t)
                    where aux [] [] = []
                          aux [] l = l
                          aux l [] = l
                          aux (x:xs) (y:ys)  | x<((y* (d l p t)) +1) = (y*(d l p t) +1) : aux xs ys 
                                             | otherwise = x : aux xs ys                                                                                                                 
                                             
{-| Esta devolve uma lista de listas com todos os números do mapa pela qual a chama passa. Assim, através de uma auxiliar recebemos o número do mapa onde se 
encontram as bombas e o número máximo do mapa até onde a chama pode proceguir, pelo que geramos uma lista cujo primeiro elemento é o número da bomba mais um e o 
último é o número máxima da chama. Recorremos à recursiva para as restantes bombas prestes a explodir.  -}
numerosR :: [String] -> Int -> Int -> [[Int]]
numerosR l p t = aux (convertToNum l p t) (restricaoR l p t)
                      where aux [] [] = []
                            aux l [] = [l]
                            aux [] l = [l]
                            aux (x:xs) (y:ys) = [[(x+1)..y]] ++ aux xs ys


{-| Esta devolve uma lista de listas com todos os números do mapa pela qual a chama passa. Pelo que, através de uma auxiliar recebemos o número do mapa onde se 
encontram as bombas e o número máximo do mapa até onde a chama pode proceguir. Para tal, recorremos a uma lista de compreensão, que cada elemento da lista será 
o número do mapa onde se encontra a bomba menos o produto da dimensão do mapa por um "a" (o que se deve ao facto de a chama se propagar para cima) tal que este
"a" vai ser cada elemento de uma lista dos números de um 
até à divisão da diferença dos números do mapa onde se encontra a bomba e da chama máxima pela dimensão deste. É necessária a recursiva para que o mesmo seja 
feito para as restantes bombas prestes a explodir.  -}
numerosU :: [String] -> Int -> Int -> [[Int]]
numerosU l p t = aux (convertToNum l p t) (restricaoU l p t)
                      where aux [] [] = []
                            aux l [] = [l]
                            aux [] l = [l]
                            aux (x:xs) (y:ys) = [[x-a*(d l p t) | a <- [1..(div (x-y) (d l p t))]]] ++ aux xs ys                                                                            

{-| Esta devolve uma lista de listas com todos os números do mapa pela qual a chama passa. Desta forma, através de uma auxiliar recebemos o número do mapa onde se 
encontram as bombas e o número máximo do mapa até onde a chama pode proceguir, pelo que geramos uma lista cujo primeiro elemento é o número máximo da chama e o último 
é o número da bomba, sendo necessário inverter esta lista uma vez que a chama inicia no local do mapa onde se encontra a bomba. Recorremos à recursiva para as 
restantes bombas prestes a explodir. -}
numerosL :: [String] -> Int -> Int -> [[Int]]
numerosL l p t = aux (convertToNum l p t) (restricaoL l p t)
                      where aux [] [] = []
                            aux l [] = [l]
                            aux [] l = [l]
                            aux (x:xs) (y:ys) =  [(reverse [y..(x-1)])] ++ aux xs ys

{-| Esta devolve uma lista de listas com todos os números do mapa pela qual a chama passa. Pelo que, através de uma auxiliar recebemos o número do mapa onde se 
encontram as bombas e o número máximo do mapa até onde a chama pode proceguir. Para tal, recorremos a uma lista de compreensão, que cada elemento da lista será 
o número do mapa onde se encontra a bomba mais o produto da dimensão do mapa por um "a" (o que se deve ao facto de a chama se propagar para baixo) tal que este 
"a" vai ser cada elemento de uma lista dos números de um até à divisão da diferença dos números do mapa onde se encontra a bomba e da chama máxima pela dimensão 
deste. É necessária a recursiva para que o mesmo seja feito para as restantes bombas prestes a explodir. -}
numerosD :: [String] -> Int -> Int -> [[Int]]
numerosD l p t = aux (convertToNum l p t) (restricaoD l p t)
                      where aux [] [] = []
                            aux l [] = [l]
                            aux [] l = [l]
                            aux (x:xs) (y:ys) = [[x+a*(d l p t) | a <- [1..(div (y-x) (d l p t))]]] ++ aux xs ys                            

{-| A função raio devolve uma lista com os raios das bombas prestes a explodir (ou seja, com tempo igual a 1). Começamos por percorrer a lista até encontrar uma bomba ('*')
e com tempo de explosão 1. Quando esta é encontrada vamos retirar-lhe a informação do seu raio e fazemos uma recursiva para verificar a existência de mais bombas nesta 
situação. Para tal, usamos a função break que permitiu separar o tempo de explosão da bomba da sua restante informação. E de seguida, pegamos no segundo elemento do tuplo 
ao qual retiramos o primeiro elemento visto que correspondia a um ' ' (contém a restante informação da bomba) e recolhemos o primeiro elemento do resultado da função break
para obtermos o raio da bomba. Caso não exista nenhuma bombas prestes a explodir devolve uma lista com raio 0.  -}
raio :: [String] -> Int -> Int -> [Int]
raio [] p t = []
raio l p t = if (reverse l) !! 0 !! 0 == '*' 
             then [(read (reverse (fst ((break (==' ') (drop 1 (snd (break (==' ') (reverse ((reverse l) !! 0))))))))) :: Int)] ++ raio (drop 1 (reverse l)) p t
             else raio (drop 1 (reverse l)) p t


{-| Esta função devolve uma lista com todos os pontos do mapa em que as chamas se poderão propagar. Ou seja, é uma lista com todos os números do mapa em que a 
chama se pode propagar, em todas as diferentes direções, e o local onde está a bomba. -}
locaisComChama :: [String] -> Int -> Int -> [Int]
locaisComChama l p t = concat((numerosD l p t)++(numerosU l p t)++(numerosR l p t)++(numerosL l p t))++(convertToNum l p t)
