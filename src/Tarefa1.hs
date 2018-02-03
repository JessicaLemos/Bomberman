{-|
 Module: Main
 Descrption: funções que permitem a construção do mapa
 Copyright: Ana Ribeiro
            Jéssica Lemos -}

module Main where 
import System.Environment
import Text.Read
import Data.Maybe
import System.Random

{-| A função "mapa" faz a junção do mapa com os PowerUps. Dado que o mapa inicia na coordenada (0,0) à dimensão retiramos um. -}
mapa :: Int -> Int -> [String]
mapa d s = (juntaLinhas (0,0) (d-1) (semente d s)) ++ (powerUps (0,0) (d-1) (semente d s))

{-| A função "locPonto" indica para cada ponto do mapa se este é pedra("#") ou vazio (""). Tivemos em atenção que os extremos do mapa,
ou seja, a primeira e ultima colunas e linhas são pedras. Posteriormente, recorremos à função "locVazios" que indica as posições 
dos vazios pré-definidos. Além disto, verificamos que se a linha e a coluna forem pares encontra-se também uma pedra. Nos restantes 
casos, verificamos se o conteúdo de cada ponto corresponde a um PowerUp ('+' ou '!'), um tijolo ('?') ou um espaço vazio (' '). -}
locPonto :: (Int,Int) -> Int -> [Int] -> [Char]
locPonto (x,0) d l = "#"                                                               
locPonto (0,y) d l = "#" 
locPonto (x,y) d l | (x==d || y==d) = "#"
		               | locVazios (x,y) d = " "
		               | (mod x 2==0 && mod y 2==0) = "#"
	                 | otherwise = aConteudo l

{-| "aConteudo" associa o número, proveniente do cálculo de numeros aleatório recorrendo à função "semente", a um determinado conteúdo. 
Caso o número seja menor que 0 e 1 o seu conteúdo é um PowerUp Bombs ('+'), entre 2 e 3 é um PowerUo Flames ('!'), entre 3 e 39 é um 
tijolo e osrestantes é um espaço vazio. -}
aConteudo :: [Int] -> [Char]
aConteudo [] = " "
aConteudo l | ((head l>=0) && (head l<=1)) = "+"
            | ((head l>1) && (head l<=3)) = "!"
            | ((head l>3) && (head l<=39)) = "?"
            | ((head l>39) && (head l<=99)) = " "

{-| Esta função une todos os pontos de cada linha. Começamos por verificar se a "locPonto" é pedra ou vazio pré-definido. Se o for, 
mantemos o ponto e recorremos de novo à "locLinha" para verificar o ponto seguinte da linha. Desta forma, somamos 1 ao y e retiramos 
um valor à sua dimenção uma vez que consideramos que cada linha tem d pontos sendo que o ultimo corresponde a d=0. Daí um dos nossos 
casos de paragem ser locLinha (x,y) 0 l = "#". Caso a "locPonto" corresponda a um PowerUp, recorresmos à função "converte" uma vez 
que estes se encontram escondidos num tijolo e recorremos novamente à "locLinha". Note-se que neste caso eliminamos um elemento da 
lista que contém os números aleatórios, dado que o primeiro número já foi associado. Aquando a "locPonto" está associada a um vazio
mantemos o ponto e fazemos a recursiva pelo mesmo métedo anterior. Quando a lista dos números aleatórios se tornar vazia prosseguimos 
seguindo o mesmo processo.-}
locLinha :: (Int,Int) -> Int -> [Int] -> [Char]
locLinha (x,y) 0 l = "#" 
locLinha (x,y) d [] = (locPonto (x,y) (d+y) []) ++ (locLinha (x,y+1) (d-1) [])
locLinha (x,y) d l | ((locPonto (x,y) (d+y) l) == "#" || (locVazios (x,y) (d+y))) = (locPonto (x,y) (d+y) l) ++ (locLinha (x,y+1) (d-1) l)
                   | ((locPonto (x,y) (d+y) l) == "+" || (locPonto (x,y) (d+y) l) == "!") = (converte (x,y) (d+y) l) ++ (locLinha (x,y+1) (d-1) (drop 1 l))
                   | otherwise = (locPonto (x,y) (d+y) l) ++ (locLinha (x,y+1) (d-1) (drop 1 l))


{-| A "juntaLinhas" faz a junção de todas as linhas, recorrendo ao aumento de x. Enquanto a lista de números aleatórios não for vazia 
vamos eliminando o número de pontos de cada linha que já foram associados. Dando origem ao mapa --}
juntaLinhas :: (Int,Int) -> Int -> [Int] -> [String]
juntaLinhas (x,y) 0 l = []
juntaLinhas (x,y) d [] = if x>d 
                           then []
                           else (locLinha (x,y) d []):(juntaLinhas (x+1,y) d [])
juntaLinhas (x,y) d l = if x>d 
                          then [] 
                          else (locLinha (x,y) d l):(juntaLinhas (x+1,y) d (drop (nCelulasLinha (x,y) d l) l))

{-| A função dá os PowerUps de determinados pontos. Caso a "locPonto" seja pedra ou um vazio pré-definido devolve-nos a lista vazia, 
uma vez que não contém PowerUps. O mesmo se verifica caso a "locPonto" não seja um PowerUp. Se "locPonto" corresponder a um PowerUp 
ao símbolo que lhe correspondo juntamos a coordenada do y e do x, utilizando a função show e seguindo a formatação definida. -}
pontoPowerUp :: (Int,Int) -> Int -> [Int] -> [[Char]]
pontoPowerUp (x,y) d [] = []
pontoPowerUp (x,y) d l | ((locPonto (x,y) d l)== "#" || (locVazios (x,y) d)) = [] 
                       | ((locPonto (x,y) d l) == "+" ) = ["+" ++ " " ++ (show y) ++ " " ++ (show x)]
                       | ((locPonto (x,y) d l) == "!" ) = ["!" ++ " " ++ (show y) ++ " " ++ (show x)] 
                       | otherwise = []

{-| Esta função junta todos os Bombs de determinada linha. Se a "locPonto" contiver um Bomb então recorre à "pontoPowerUp" e prossegue 
para o ponto seguinte (daí o y aumentar, a dimensão diminuir e ser eliminada a cabeça da lista). Aquando o ponto corresponde a uma pedras
ou vazio pré-definido, a função segue para o seguinte sem eliminar um elemento da lista (por não ser utilizado). Nas seguintes situações 
recorremos novamente à "locBombs" para verificar o ponto seguinte com a eliminação de cabeças da lista. É de salientar que sempre que 
chamamos a "locPonto" ao d somamos o y de modo a compensar a redução da dimensão aplicada na recursividade da locBombs. -}
locBombs :: (Int,Int) -> Int -> [Int] -> [String]
locBombs (x,y) 0 l = []
locBombs (x,y) d [] = []
locBombs (x,y) d l | (((locPonto (x,y) (d+y) l)) == "+") = (pontoPowerUp (x,y) (d+y) l) ++ locBombs (x,y+1) (d-1) (drop 1 l)
                   | ((locPonto (x,y) (d+y) l) == "!") = locBombs (x,y+1) (d-1) (drop 1 l)
                   | (((locPonto (x,y) (d+y) l) == "#") || (locVazios (x,y) (d+y))) = locBombs (x,y+1) (d-1) l
                   | ((locPonto (x,y) (d+y) l) == " ") = locBombs (x,y+1) (d-1) (drop 1 l)
                   | ((locPonto (x,y) (d+y) l) == "?") = locBombs (x,y+1) (d-1) (drop 1 l)

{-| Esta junta os Bombs de todas linhas, recorrendo ao aumento de x. Isto é, a função recorre à "locBombs" e faz a recursiva para a 
linha seguinte. Enquanto a lista de números aleatórios não for vazia vamos eliminando o número de pontos de cada linha que já foram 
associados. Quando a lista de número aleatórios for vazia devolve a lista vazia dado que, não existem mais Bombs para juntar. -}
juntaBombs :: (Int,Int) -> Int -> [Int] -> [String]
juntaBombs (x,y) d [] = []
juntaBombs (x,y) d l = if x>d 
                        then [] 
                        else (locBombs (x,y) d l)++(juntaBombs (x+1,y) d (drop (nCelulasLinha (x,y) d l) l)) 

{-| Esta função junta todos os Flames de determinada linha. Se a "locPonto" contiver um Flame então recorre à "pontoPowerUp" e prossegue 
para o ponto seguinte (daí o y aumentar, a dimensão diminuir e ser eliminada a cabeça da lista). Aquando o ponto corresponde a uma pedras
ou vazio pré-definido, a função segue para o seguinte sem eliminar um elemento da lista (por não ser utilizado). Nas seguintes situações 
recorremos novamente à "locBombs" para verificar o ponto seguinte com a eliminação de cabeças da lista. É de salientar que sempre que 
chamamos a "locPonto" ao d somamos o y de modo a compensar a redução da dimensão aplicada na recursividade da locBombs. -} 
locFlames :: (Int,Int) -> Int -> [Int] -> [String]
locFlames (x,y) 0 l = []
locFlames (x,y) d [] = []
locFlames (x,y) d l | (((locPonto (x,y) (d+y) l)) == "!") = (pontoPowerUp (x,y) (d+y) l) ++ locFlames (x,y+1) (d-1) (drop 1 l)
                    | ((locPonto (x,y) (d+y) l) == "+") = locFlames (x,y+1) (d-1) (drop 1 l)
                    | (((locPonto (x,y) (d+y) l) == "#") || (locVazios (x,y) (d+y))) = locFlames (x,y+1) (d-1) l
                    | ((locPonto (x,y) (d+y) l) == " ") = locFlames (x,y+1) (d-1) (drop 1 l) 
                    | ((locPonto (x,y) (d+y) l) == "?") = locFlames (x,y+1) (d-1) (drop 1 l) 

{-| Esta junta os Flames de todas linhas, recorrendo ao aumento de x. Isto é, a função recorre à "locFlames" e faz a recursiva para a 
linha seguinte. Enquanto a lista de números aleatórios não for vazia vamos eliminando o número de pontos de cada linha que já foram 
associados. Quando a lista de número aleatórios for vazia devolve a lista vazia dado que, não existem mais Flames para juntar. -}
juntaFlames :: (Int,Int) -> Int -> [Int] -> [String]
juntaFlames (x,y) d [] = []
juntaFlames (x,y) d l = if x>d 
                        then [] 
                        else (locFlames (x,y) d l)++(juntaFlames (x+1,y) d (drop (nCelulasLinha (x,y) d l) l)) 

{-| A "powerUps" junta todos os Bombs e Flames, ordenados como pedido. -}
powerUps :: (Int,Int) -> Int -> [Int] -> [String]
powerUps (x,y) 0 l = []
powerUps (x,y) d l = (juntaBombs (x,y) d l) ++ (juntaFlames (x,y) d l)

{-| Esta converte os símbolos dos PowerUps em tijolos, visto que estes aparecem escondidos atrás de um tijolo no mapa. -}
converte :: (Int,Int) -> Int -> [Int] -> [Char]
converte (x,y) d [] = " "
converte (x,y) d l | (locPonto (x,y) d l == "+") = "?"
                   | (locPonto (x,y) d l == "!") = "?"

{-| "nCelulasLinha" calcula o número de números aleatórios numa determinada linha. Note-se que a dimensão é chamada com d-1. 
A primeira e última linha não necessita de nenhum, uma vez que é pedra. Na segunda e penúltima linhas à dimensão retiramos 
cinco, ou seja, dois vazios pré-definidos e duas pedras. Se a linha for impar à dimensão refiramos um, ou seja, as duas pedras
dos extremos. No caso da terceira e antepenúltima linhas calculamos através da expressão (d+1)-((div (d+2) 2) +2), em que d 
corresponde à dimensão. Nos restantes casos, (d+1)-(div (d+2) 2). -}
nCelulasLinha :: (Int,Int) -> Int -> [Int] -> Int
nCelulasLinha (x,y) d [] = 0
nCelulasLinha (x,y) d l | x==0 || x==d = 0
                        | (x==1 || x==(d-1)) = d-5
                        | (mod x 2==1) = (d-1)
                        | (x==2 || x==(d-2)) = (d+1)-((div (d+2) 2) +2)
                        | otherwise = (d+1)-(div (d+2) 2)

{-| Esta função indica o número de pedras do mapa, recorrendo apenas ao tamanho do mapa. -}
nPedras :: Integral a => a -> a
nPedras d = 2*d + (d-2)*2 + (div(d-3) 2)*((div (d+1) 2) -2)

{-| A função nCelulas dá o número de céluas que irão ser associadas a um número. Para tal ao número total de pontos do mapa 
retiramos as pedras e os vazios pré-definidos. -} 
nCelulas :: Integral a => a -> a
nCelulas d = d*d - nPedras d - 12

{-| A "semente" gera um conjunto de números aleatórios tendo em conta a semente. Usamos a função "nCelulas" uma vez que apenas 
serão associados números aleatórios a estes pontos. -}
semente ::  Int -> Int -> [Int] 
semente d s = take (nCelulas d) $ randomRs (0,99) (mkStdGen s)

{-| A função "locVazios" indica as posições do vazios pré-definidos, tendo em conta que os vazios pré-definidos apenas se 
encontram nas linha um, dois, d-1 e d-2 e nas colunas um, dois, d-1 e d-2. Tivemos em atenção que as colunas não podem ser 
ambas dois nem d-2. -}
locVazios :: (Int,Int) -> Int -> Bool
locVazios (x,y) d =
    ((x==1) && (y==1 || y==2 || y==(d-1) || y==(d-2))) || 
    ((x==2) && (y==1 || y== d-1)) ||
    (x==(d-1)) && (y==1 || y==2 || y==(d-1) || y==(d-2)) ||
    ((x==(d-2)) && (y==1 || y== d-1))

main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"


