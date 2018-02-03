{-|
 Module: Main
 Descrption: funções que permitem o movimento dos jogadores no mapa
 Copyright: Ana Ribeiro
            Jéssica Lemos -}

module Main where
import Data.List
import Data.Char 
import System.Environment
    

{-| Caso não seja possível aplicar o movimente quer por não existir o jogador, ou o ponto para o qual pretende se deslocar seja uma pedra ou um tijolo ou o comando bomba seja aplicado a um jogador que não tem PowerUps, é devolvido o próprio mapa. Se for executado o comando bomba é chamada a função "mapaMoveB". Caso o ponto para onde pretende ir o jogador coincida com um PowerUp devolve a "mapaMove" pela ordem contrária. Nos restantes casos, devolve também a "mapaMove" por ordem contrária mais os restantes jogadores. -}
move :: [String] -> Int -> Char -> [String]
move [] jog com = []
move l jog com | ((veJogador l jog com) == False) = l
               | ((pPonto l jog com) == '#' || (pPonto l jog com) == '?') = l
               | ((com == 'B') && ((existPU l jog com)== False)) = l
               | (com == 'B') = mapaMoveB l jog com
               | elem (preveMov l jog com) (coordNumPowerUpF l jog com) || elem (preveMov l jog com) (coordNumPowerUpB l jog com) = reverse (mapaMove l jog com)
               | otherwise = reverse ((mapaMove l jog com)) ++ reverse(take ((length l) - (length (mapaMove l jog com)))  (reverse l))

{-| Esta função verifica se o jogador está em jogo. Para tal percorremos a lista, através de uma recursiva, até encontrarmos a lista que contém um algarismo. Caso o algarismo corresponda ao algarismo do jogador ao qual foi aplicado o movimento então a função devolve True. Caso contrário a lista tornar-se-á vazia e desoverá False.-}
veJogador :: [String] -> Int -> Char -> Bool
veJogador [] jog com = False
veJogador l jog com = if ((head (head (take 1 l)))>= '0' && (head (head (take 1 l))<= '9'))   
                  then (if (digitToInt (head (head (take 1 l))) == jog) then True else veJogador (drop 1 l) jog com)
                  else veJogador (drop 1 l) jog com
 
{-| "infJogador" dá a informação relativa a um jogador. Para tal percorremos a lista, através de uma recursiva, até encontrarmos a lista que contém um algarismo. Caso o algarismo corresponda ao algarismo do jogador ao qual foi aplicado o movimento então é devolvida a lista com a informação do jogador. -}
infJogador :: [String] -> Int -> Char -> [String]
infJogador [] jog com = []
infJogador l jog com = if ((head (head (take 1 l)))>= '0' && (head (head (take 1 l))<= '9'))
                      then (if (digitToInt (head (head (take 1 l))) == jog) then take 1 l else infJogador (drop 1 l) jog com)
                      else infJogador (drop 1 l) jog com

{-| Esta indica a dimensão do mapa recebido. Para tal calculamos o tamanho da cabeça da lista, que corresponde à primeira linha do mapa constituída apenas por pedras. -}
dim :: [String] -> Int -> Char -> Int
dim l jog com = length (head (take 1 l))

{-| Indica o numero do mapa correspondente à sua coordenada. Assim, multiplicamos o tamanho do mapa pela linha onde se encontra o jogador e somamos com a coluna mais um. -}
coorJogNum :: [String] -> Int -> Char -> Int
coorJogNum l jog com = (dim l jog com) *(digitToInt (head (drop 4 (head (infJogador l jog com))))) + ( digitToInt (head (drop 2 (head (infJogador l jog com))))+1)

{-| "pPonto" é o caracter associado à posição para a qual o jogador pretende ir.  -}
pPonto :: [String] -> Int -> Char -> Char
pPonto l jog com = (head (drop ((mod (preveMov l jog com) (dim l jog com)) -1) (head (take 1 (drop (div (preveMov l jog com) (dim l jog com)) l)))))

{-| Esta dá a posição do mapa para qual o jogador pretende ir. Se o comando for 'B' então a coordenada correspondente é a mesma do jogador. Caso seja 'R' o jogador movimenta-se para a direita logo a sua posição será o número atual do jogador menos um. O contrário acontece quando o jogador se movimenta para a esquerda 'L'. Se o jogador pretender ir para cima a sua posição será o seu número menos a dimensão do mapa. Quando pretende ir para baixo, é o contrário. -}
preveMov :: [String] -> Int -> Char -> Int
preveMov l jog com | (com == 'U') = (coorJogNum l jog com) - (dim l jog com)
                   | (com == 'D') = (coorJogNum l jog com) + (dim l jog com)
                   | (com == 'L') = (coorJogNum l jog com) - 1
                   | (com == 'R') = (coorJogNum l jog com) + 1
                   | (com == 'B') = (coorJogNum l jog com)

{-| A função dá a lista com as novas coordenadas do jogador. O y é dado pelo resto da divisão do número da posição para a qual vai pela dimensão do mapa, menos um. O x é obtido pela divisão inteira do número da posição para a qual vai pela dimensão do mapa. -}
coorNumJog :: [String] -> Int -> Char -> [Char]
coorNumJog l jog com = [intToDigit ((mod (preveMov l jog com) (dim l jog com)) -1)] ++ " " ++ [intToDigit (div (preveMov l jog com) (dim l jog com))]

{-| A "mapaMove" junta a alteração da informação do jogador ao mapa. No caso da posição para a qual o jogador vai corresponder a um PowerUp Bombs e se o jogador tiver PowerUps, então percorremos a lista até encontrarmos o número do jogador que se vai movimentar. Enquanto tal não se verifica a função vai armazenando as listas eliminadas pela recursividade da função que não contém a lista do PowerUp apanhado pelo jogador, o que é assegurado pela utilização da função "mapaSPUb". Quando for encontrado junta o identificador do jogador com as coordenadas para qual vai e o restante da lista acrescentando o símbolo de '+' mais o restante mapa sem a informação antiga do jogador.      
Caso o jogador vá para uma posição na qual haja PowerUps e o jogador não tenha PowerUps então recorremos ao mesmo método utilizado anteriormente com a diferença de haver um espaço entre a coordenada x e o símbolo de Bombs. O mesmo acontece no caso de o jogador se movimentar para o local onde houver PowerUps Flames apenas se diferençiando na medida em que o seu símbolo é '!'. Caso o jogador não se movimente para um local onde haja PowerUps utilizamos uma recursiva até encontrarmos a lista com a informação do jogador. De seguida alteramos esta informação mundando apenas as suas coordenadas, juntando posteriormente ao resto da lista. -}
mapaMove :: [String] -> Int -> Char -> [String]
mapaMove l jog com | ((elem (preveMov l jog com) (coordNumPowerUpB l jog com)) && (existPU l jog com)== True) = if digitToInt (head (head (take 1 (reverse (mapaSPUb l jog com))))) == jog
                                                                                                               then [[head (head (take 1 (reverse (mapaSPUb l jog com))))] ++ " " ++ coorNumJog l jog com ++ (drop 5 (head ((reverse (mapaSPUb l jog com)))))++ "+"] ++ (drop 1 (reverse (mapaSPUb l jog com))) 
                                                                                                                 else take 1 (mapaSPUb (reverse l) jog com) ++ mapaMove (reverse(drop 1 (reverse l))) jog com
                  
                   | elem (preveMov l jog com) (coordNumPowerUpB l jog com) = if digitToInt (head (head (take 1 (reverse (mapaSPUb l jog com))))) == jog
                                                                              then [[head (head (take 1 (reverse (mapaSPUb l jog com))))] ++ " " ++ coorNumJog l jog com ++ (drop 5 (head ((reverse (mapaSPUb l jog com)))))++ " +"] ++ (drop 1 (reverse (mapaSPUb l jog com))) 
                                                                              else take 1 (mapaSPUb (reverse l) jog com) ++ mapaMove (reverse(drop 1 (reverse l))) jog com
                  
                   | ((elem (preveMov l jog com) (coordNumPowerUpF l jog com)) && (existPU l jog com)== True) = if digitToInt (head (head (take 1 (reverse (mapaSPUf l jog com))))) == jog
                                                                                                                 then [[head (head (take 1 (reverse (mapaSPUf l jog com))))] ++ " " ++ coorNumJog l jog com ++ (drop 5 (head ((reverse (mapaSPUf l jog com)))))++ "!"] ++ (drop 1 (reverse (mapaSPUf l jog com))) 
                                                                                                                 else take 1 (mapaSPUf (reverse l) jog com) ++ mapaMove (reverse(drop 1 (reverse l))) jog com
                  
                   | elem (preveMov l jog com) (coordNumPowerUpF l jog com) = if digitToInt (head (head (take 1 (reverse (mapaSPUf l jog com))))) == jog
                                                                              then [[head (head (take 1 (reverse (mapaSPUf l jog com))))] ++ " " ++ coorNumJog l jog com ++ (drop 5 (head ((reverse (mapaSPUf l jog com)))))++ " !"] ++ (drop 1 (reverse (mapaSPUf l jog com))) 
                                                                              else take 1 (mapaSPUf (reverse l) jog com) ++ mapaMove (reverse(drop 1 (reverse l))) jog com
                  
                   | otherwise = if take 1 (reverse l) == infJogador l jog com 
                                    then  [[head (head (take 1 (reverse l)))] ++ " " ++ coorNumJog l jog com ++ (drop 5 (head (reverse l)))] ++ (drop 1 (reverse l))
                                    else (mapaMove (reverse(drop 1 (reverse l))) jog com) 

{-| Esta função devolve-nos uma lista com todos os Bombs descobertos no mapa. Percorremos a lista até encontrar-mos um Bombs, quando tal se sucede armazenamo-lo e verificamos se existem mais tarvés da recursiva. A função termina quando todos elementos foram verificados (lista vazia).-}
coordNumPUB :: [String] -> Int -> Char -> [String]
coordNumPUB [] jog com = []
coordNumPUB l jog com = if head (head (take 1 (reverse l))) == '+'
                            then take 1 (reverse l) ++ coordNumPUB (drop 1 (reverse l)) jog com
                            else coordNumPUB (reverse(drop 1 (reverse l))) jog com

{-| Esta função devolve-nos uma lista com todos os Flames descobertos no mapa. Percorremos a lista até encontrar-mos um Flames, quando tal se sucede armazenamo-lo e verificamos se existem mais tarvés da recursiva. A função termina quando todos elementos foram verificados (lista vazia).-}
coordNumPUF :: [String] -> Int -> Char -> [String]
coordNumPUF [] jog com = []
coordNumPUF l jog com = if head (head (take 1 (reverse l))) == '!'
                            then take 1 (reverse l) ++ coordNumPUF (drop 1 (reverse l)) jog com
                            else coordNumPUF (reverse(drop 1 (reverse l))) jog com

{-| A função dá uma lista com os Bombs descobertos. Esta percorre todos os elementos da lista e aplica-lhes a função f, que multiplica o tamanho do mapa pela linha onde se encontra o Bombs e soma-lhe a coluna mais um.-}
coordNumPowerUpB :: [String] -> Int -> Char -> [Int]
coordNumPowerUpB l jog com = map f (coordNumPUB l jog com)
                     where f h = ((dim l jog com) * (digitToInt (head (drop 4 h))) + (digitToInt (head (drop 2 h)))+1)

{-| A função dá uma lista com os Flames descobertos. Esta percorre todos os elementos da lista e aplica-lhes a função f, que multiplica o tamanho do mapa pela linha onde se encontra o Flames e soma-lhe a coluna mais um.-}
coordNumPowerUpF :: [String] -> Int -> Char -> [Int]
coordNumPowerUpF l jog com = map f (coordNumPUF l jog com)
                     where f h = ((dim l jog com) * (digitToInt (head (drop 4 h))) + (digitToInt (head (drop 2 h)))+1) 

{-| A "mapaSPUb" devolve o estado do mapa inicial sem a informação referente ao Bombs apanhado pelo jogador, recorrendo à função pré-definida (\\). -}
mapaSPUb :: [String] -> Int -> Char -> [String]
mapaSPUb [] jog com = []
mapaSPUb l jog com = l \\ ["+ " ++ (coorNumJog l jog com)]

{-| A "mapaSPUf" devolve o estado do mapa inicial sem a informação referente ao Flame apanhado pelo jogador, recorrendo à função pré-definida (\\). -}
mapaSPUf :: [String] -> Int -> Char -> [String]
mapaSPUf [] jog com = []
mapaSPUf l jog com = l \\ ["! " ++ (coorNumJog l jog com)] 

{-| "existPU" verifica se o jogador tem powerUps para colocar bombas. -}
existPU :: [String] -> Int -> Char -> Bool
existPU [] jog com = False
existPU l jog com | elem '+' (head (infJogador l jog com)) || elem '!' (head (infJogador l jog com)) = True
                  | otherwise = False          

{-| Esta diz se existe bomba no mapa do estado de jogo inicial. Percorremos a lista, através de uma recursiva, caso seja encontrado uma lista 
que contém o símbolo da bomba, é porque existe caso contrário a lista ficará vazia devolvendo False. -}
existBomb ::[String] -> Int -> Char -> Bool
existBomb [] jog com = False
existBomb l jog com = if head(head (take 1 (reverse l)))=='*'
                        then True
                        else existBomb (reverse(drop 1 (reverse l))) jog com

{-| Esta função dá a informação da bomba proveniente do mapa do estado do jogo inicial. Para tal percorremos a lista, através de uma recursiva,
até encontrarmos a lista que contém o símbolo da bomba ('*'). Quando tal se sucede é devolvida esta lista. Para evitar problemas com a dimensão 
do mapa, invertemos a lista. -}
infBomb :: [String] -> Int -> Char -> [String]
infBomb [] jog com = []
infBomb l jog com = if head (head (take 1 (reverse l)))== '*'
                      then [head (take 1 (reverse l))]
                      else infBomb (drop 1 (reverse l)) jog com

{-| A função indica a informação da bomba que o jogador colocou. Esta é constituída com o símbolo da bomba, a coordenada do jogador que a colocou
(dada pela função "coorNumJog"), o caracter coorrespondente ao jogador que a colocou, o raio de ação e tempo de explosão já definidos. Tivemos 
também em conta de que se o jogador tiver um PowerUp Flame o raio da bomba será 2. -}
infNovaBomb :: [String] -> Int -> Char -> [String]
infNovaBomb l jog com | head(reverse (head (infJogador l jog com)))=='!'  = ["* " ++ (coorNumJog l jog com) ++ " " ++ [intToDigit jog] ++ " 2 10"]
                      | otherwise = ["* " ++ (coorNumJog l jog com) ++ " " ++ [intToDigit jog] ++ " 1 10"]

{-| Esta função junta as bombas ordenadamente no mapa do estado inicial.Se o jogador pretender colocar uma bomba num local onde já existe entao é lhe devolvido o mapa anterior. Nos restantes casos em que existe bomba, juntamos a "mapaMe" seguida das bombas ordenadas e da "mapaTu" por ordem contrária.Caso contrário unimos o "mapaTi" com a bomba nova e com o "mapaTo" por ordem contrário.-}
mapaMoveB :: [String] -> Int -> Char -> [String]
mapaMoveB l jog com 
     | ((posBomb l jog com)== l && (existBomb l jog com)== True) = l
     | (existBomb l jog com)== True = (mapaMe l jog com) ++ (posBomb l jog com) ++ reverse (mapaTu l jog com)

     | (existBomb l jog com)== False = (mapaTi l jog com) ++ (infNovaBomb l jog com) ++ reverse (mapaTo l jog com)

{-| A função ordena as bombas, ou seja, ordena a bomba proveniente do estado do mapa inicial e a bomba colocada pelo jogador. Caso não haja 
bomba no estado do mapa inicial é devolvida apenas a informação da nova bomba. Senão é comparado o número associado à bomba anterior com o 
número associado à nova bomba. É de notar que quando o jogador pretende colocar uma bomba onde já existe uma bomba é devolvido o estado do 
mapa anterior.  -}
posBomb :: [String] -> Int -> Char -> [String]
posBomb [] jog com = []
posBomb l jog com | ((existBomb l jog com)== False) = (infNovaBomb l jog com)
                  | (coorBombNum l jog com)<(coorBombExisNum l jog com) = (infNovaBomb l jog com) ++ (infBomb l jog com)
                  | (coorBombNum l jog com)>(coorBombExisNum l jog com) = (infBomb l jog com) ++ (infNovaBomb l jog com) 
                  | (coorBombNum l jog com)==(coorBombExisNum l jog com) = l

{-| A função indica o número do mapa da bomba colocada que o jogador quer colocar. Assim, multiplicamos o tamanho de mapa pela linha onde se encontra a bomba que vai 
ser colocada e somamos com a coluna mais um. -}
coorBombNum :: [String] -> Int -> Char -> Int
coorBombNum l jog com = (dim l jog com) *(digitToInt (head (drop 4 (head (infNovaBomb l jog com))))) + (digitToInt (head (drop 2 (head (infNovaBomb l jog com))))+1)

{-| A função indica o número do mapa da bomba proveniente do estado inicial. Assim, multiplicamos o tamanho de mapa pela linha onde se encontra a bomba e somamos 
com a coluna mais um. -}
coorBombExisNum :: [String] -> Int -> Char -> Int
coorBombExisNum l jog com = (dim l jog com) *(digitToInt (head (drop 4 (head (infBomb l jog com))))) + (digitToInt (head (drop 2 (head (infBomb l jog com))))+1)

{-| A "mapaMe" percorre a lista e guarda a informação até encontrar a informação da bomba ("infBomb"). -}
mapaMe :: [String] -> Int -> Char -> [String]
mapaMe [] jog com = []
mapaMe l jog com = if take 1 l == (infBomb l jog com)
                   then []
                   else (take 1 l) ++ mapaMe (drop 1 l) jog com
{-| Esta função percorre todos os elementos da lista, no entanto inverte-a, para armazenar a informação posterior à bomba. -}
mapaTu :: [String] -> Int -> Char -> [String]
mapaTu [] jog com = []
mapaTu l jog com = if take 1 (reverse l) == (infBomb l jog com)
                   then []
                   else (take 1 (reverse l)) ++ mapaTu (reverse(drop 1 (reverse l))) jog com
{-| A "mapaTi" percorre a lista e guarda a informação até encontrar um algarismo, fornecendo um mapa sem informações dos jogadores.-}
mapaTi :: [String] -> Int -> Char -> [String]
mapaTi [] jog com = []
mapaTi l jog com = if ((head (head (take 1 l))>= '0' && (head (head (take 1 l)))<= '3'))
                     then []
                     else (take 1 l) ++ mapaTi (drop 1 l) jog com
{-| A função percorre todos os elementos da lista, no entanto inverte-a, para armazenar a informação dos jogadores.-}
mapaTo :: [String] -> Int -> Char -> [String]
mapaTo [] jog com = []
mapaTo l jog com = if (head (head (take 1 (reverse l)))>= '0' && (head (head (take 1 (reverse l))))<= '3')
                     then take 1 (reverse l) ++ mapaTo (reverse (drop 1 (reverse l))) jog com
                     else []

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"
