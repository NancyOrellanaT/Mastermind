{-# OPTIONS_GHC -fno-warn-tabs #-}
-- ===========================================================================
-- Proyecto Final
-- Programacion IV - Programacion Funcional
-- Departamento de Sistemas Computacionales
-- Universidad Privada Boliviana
-- Nombre: Adriana Nancy Orellana Torrico                Código = 40665
-- ===========================================================================
import System.Random
import Data.List

data Color = Rojo | Amarillo | Naranja | Rosado | Verde | Azul | Violeta | Celeste | Gris | Cafe | Blanco | Negro | Vacio deriving (Eq,Ord,Show)

-- Establece el tamaño de la lista de colores
cantidad = 4

-- Generar una lista de n numeros random unica
gLNumeros::Int->[Int]->[Int]
gLNumeros 0 _ = []
gLNumeros _ [] = []
gLNumeros n xs = take n (nub (xs))

-- Generador de lista de colores según una lista de números aleatorios que no contienen repetidos
gLColores::Int->[Int]->[Color]
gLColores 0 _ = []
gLColores _ [] = []
gLColores n (x:xs) = (anadirColor x):(gLColores (n - 1) xs) 

-- Elige un color según un número
anadirColor::Int->Color
anadirColor n | n == 0 = Rojo | n == 1 = Naranja | n == 2 = Amarillo | n == 3 = Rosado | n == 4 = Verde | n == 5 = Azul |  n == 6  = Violeta | n == 7 = Celeste | n == 8 = Gris | otherwise = Cafe

-- Se revisa si la lista es única, de otra forma se tratará de un error
listaUnica::[Color]->Bool
listaUnica [] = True
listaUnica (x:xs) | notElem x xs = True && (listaUnica xs)
                  | otherwise = False

-- Evalua la entrada del jugador con la listaGenerada

evaluarEntrada::[Color]->[Color]->[Color]->[Color]
evaluarEntrada [] [] _ = []
evaluarEntrada [] xs _ = [Vacio|x<-xs]
evaluarEntrada (x:xs) (y:ys) zs | x == y = Blanco:(evaluarEntrada xs ys zs) 
                                | elem x zs = Negro:(evaluarEntrada xs ys zs)
                                | otherwise = Vacio:(evaluarEntrada xs ys zs)


-- Evalua si se gano el juego comparando la lista de pistas obtenidas
ganoElJuego::[Color]->Bool
ganoElJuego xs = and ([x == Blanco| x<-xs])

-- Convertir una lista de tipo caracter a Color
convertir::[Char]->[Color]
convertir xs = map (stringAColor) (separarPor " []," xs)

--Convierte un string a tipo Color
stringAColor::String->Color
stringAColor xs |  xs == "Rojo" = Rojo | xs == "Naranja" = Naranja | xs == "Amarillo" = Amarillo | xs == "Rosado" = Rosado | xs == "Verde" = Verde | xs == "Azul" = Azul | xs == "Violeta" = Violeta | xs == "Celeste" = Celeste | xs == "Gris" = Gris | otherwise = Cafe

-- Separa una lista de caracteres dad una lista de separadores
separarPor :: Eq a => [a] -> [a] -> [[a]]
separarPor [] xs = [xs]
separarPor xs [] = []
separarPor xs l@(y:ys) | notElem y xs = (takeWhile (\x->notElem x xs) l):(separarPor xs (dropWhile (\x->notElem x xs) ys))
                       | otherwise = separarPor xs ys

-- Obtener lista
obtenerLista::[Char]->[Char]
obtenerLista [] = []
obtenerLista xs = xs

-------------------------------------------------------------------------------------------------------------------
--Entrada - salida del juego

opcionesMenu = ["1. Un jugador vs Computadora -> Deberas adivinar una lista de 4 colores en 7 turnos.","2. Dos jugadores -> El primer jugador ingresará una lista de 4 colores y el segundo deberá adivinar en 10 turnos.","3. Instrucciones de cómo jugar","4. Salir"]
colores = "Rojo - Amarillo - Naranja - Rosado - Verde - Azul - Violeta - Celeste - Gris - Cafe"

-- Inicio menu principal
main = do
    putStrLn "-----------------------------------------------------¡Bienvenido a Mastermind!-----------------------------------------------------"
    putStrLn $ unlines opcionesMenu
    putStrLn "¿Qué opción te gustaría elegir?"
    op <- readLn::IO Int
    case op of
        1 -> do
            putStrLn "Eligió la opción 1. ¡Vamos a comenzar!"
            -- La computadora genera una palabra y el usuario debe a divinarla
            numeroGenerado <- generar
            let turnos = 7
            let lColores = gLColores cantidad numeroGenerado
            putStrLn . concat $ ["Ingresa una lista de 4 colores que creas correcta. Recuerda la lista debe ser única y puede contener los siguientes colores: ", "\n", colores]
            modo1J lColores turnos
        2-> do
            putStrLn "Eligió la opción 2. ¡Vamos a comenzar!"
            putStrLn . concat $ ["Primero el jugador 1 ingresará una lista de 4 colores y el jugador 2 deberá adivinarla. Luego será el turno del jugador 2.","\n", "Quien consiga adivinar primero durante tres partidas, gana el juego.", "\n","¡Suerte!"]
            let turnosM = 6 
            multijugador turnosM
        3-> do
            putStrLn . concat $ ["Para comenzar a jugar debes saber que las listas de 4 colores que puedes ingresar pueden estar compuestos de los siguientes colores:","\n", "\t", "\t", "\t", "\t", colores, "\n","\n", "Si eliges la opcion de un jugador entonces, tendrás 10 turnos para adivinar la lista de 4 colores. Si la adivinas ganarás", "\n","Si eliges la opcion de dos jugadores entonces el primer jugador debera ingresar la palabra y el segundo debera adivinarla"]
            putStrLn "Ahora si puedes comenzar, ¡Disfruta el juego!"
            main
        4-> do
            putStrLn "Vuelve pronto :)"
            return ()

-- El modo de juego para un jugador 
modo1J listaColores t = do
                         putStrLn "--------------------------------------------------------------------------------------------------------"
                         if (t > 0)
                            then do
                                  putStr "Tu piensas que la respuesta es: "
                                  r <- getLine
                                  if (r /= "")
                                    then do 
                                        let respuesta = convertir (r)
                                        if (listaUnica respuesta && (length respuesta == 4))
                                            then do
                                                let similitudes = (evaluarEntrada respuesta listaColores listaColores)
                                                if (ganoElJuego similitudes)
                                                    then do
                                                        putStrLn "¡Felicidades!, ganaste el juego :D"
                                                else do
                                                    putStrLn "Ingresa nuevamente una lista de colores"
                                                    putStr "Turnos faltantes: "
                                                    print (show (t - 1))
                                                    putStr "Pista: "
                                                    print similitudes
                                                    modo1J listaColores (t - 1)
                                        else do
                                            putStrLn "Datos ingresados incorrectamente, asegurate que la lista solo contenga la cantidad los colores y establecidos en el juego."
                                            modo1J listaColores t
                                            putStr "Turnos faltantes: "
                                            print t
                                  else do
                                    putStrLn "Debes ingresar una lista con cuatro elementos, para poder adivinar y ganar."
                                    modo1J listaColores t           
                         else do
                            putStrLn "Lo sentimos, perdiste :("
                            putStr "La respuesta era: "
                            print listaColores

-- modo2Jugadores: El primer jugador ingresa la palabra y el otro debe adivinarla
multijugador turnos = do
                      if turnos > 0
                        then do
                            if even turnos
                                then do
                                    putStrLn "J1->Ingresa la lista de colores que el otro jugador deberá adivinar: "
                                    lC <- getLine
                                    putStrLn . concat $ ["\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n"]
                                    let lista = convertir lC
                                    modo1J lista 5
                                    multijugador (turnos - 1)
                              else do
                                    putStrLn "J2->Ingresa la lista de colores que el otro jugador deberá adivinar: "
                                    lC <- getLine
                                    putStrLn . concat $ ["\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n"]
                                    let lista = convertir lC
                                    modo1J lista 5
                                    multijugador (turnos - 1)
                      else do
                            putStrLn "El juego ha terminado"


-- Genera una lista de 4 elementos unicos
generar::IO [Int]
generar = do
      gen <- newStdGen
      let numeros = (randomRs (0,9) gen::[Int])
      return (gLNumeros cantidad numeros)



