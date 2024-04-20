-- | Este módulo implementa un juego de Sudoku simple en Haskell.
module Sudoku (
    -- * Tipos de datos
    Board,      -- Tipo de datos para el tablero de Sudoku
    Position,   -- Tipo de datos para representar una posición en el tablero
    
    -- * Funciones principales
    main,       -- Función principal que inicia el juego de Sudoku
    
    -- * Generación y manipulación del tablero
    plantillaInicial,   -- Plantilla inicial del tablero
    plantillaSolucion,  -- Solución de la plantilla inicial
    generarTablero,     -- Genera un tablero de Sudoku a partir de una plantilla
    actualizarTabla,    -- Actualiza una tabla de Sudoku con un nuevo valor
    
    -- * Impresión del tablero
    imprimirTablero,    -- Imprime el tablero de Sudoku
    
    -- * Juego
    jugar               -- Función principal que maneja el juego de Sudoku
) where

import Data.List
import System.Random
import Control.Monad (replicateM)

-- | Representa un tablero de Sudoku como una matriz 9x9 de enteros.
type Board = [[Int]]

-- | Representa una posición en el tablero de Sudoku, dada por fila y columna.
type Position = (Int, Int)

plantillaInicial :: Board
plantillaInicial =
    [ [5, 0, 0, 0, 0, 9, 7, 6, 0]
    , [0, 0, 4, 0, 8, 0, 0, 1, 0]
    , [0, 0, 2, 6, 0, 0, 0, 9, 0]
    , [0, 0, 0, 0, 0, 8, 0, 0, 0]
    , [6, 0, 9, 2, 0, 5, 4, 0, 3]
    , [0, 0, 0, 4, 0, 0, 0, 0, 0]
    , [0, 1, 0, 0, 0, 2, 6, 0, 0]
    , [0, 9, 0, 0, 4, 0, 5, 0, 0]
    , [0, 5, 6, 8, 0, 0, 0, 0, 9]
    ]

plantillaSolucion :: Board
plantillaSolucion =
    [ [5, 3, 8, 1, 2, 9, 7, 6, 4]
    , [9, 6, 4, 5, 8, 7, 3, 1, 2]
    , [1, 7, 2, 6, 3, 4, 8, 9, 5]
    , [3, 4, 5, 7, 9, 8, 1, 2, 6]
    , [6, 8, 9, 2, 1, 5, 4, 7, 3]
    , [7, 2, 1, 4, 6, 3, 9, 5, 8]
    , [8, 1, 3, 9, 5, 2, 6, 4, 7]
    , [2, 9, 7, 3, 4, 6, 5, 8, 1]
    , [4, 5, 6, 8, 7, 1, 2, 3, 9]
    ]


-- | Función principal que inicia el juego de Sudoku.
main :: IO ()
main = do
    putStrLn "Bienvenido al juego de Sodoku :) "
    board <- generarTablero plantillaInicial
    imprimirTablero board
    jugar board

-- | Genera un tablero de Sudoku a partir de una plantilla inicial, rellenando los espacios en blanco con números aleatorios.
generarTablero :: Board -> IO Board
generarTablero plantilla = do 
    seed <- newStdGen
    let posicionesVacias = [(fila, columna) | fila <- [1..9], columna <- [1..9], (plantilla !! (fila - 1)) !! (columna - 1) == 0]
        numerosAleatorios = take (length posicionesVacias) $ randomRs (1, 9) seed :: [Int]
        tableroConNumeros = foldl (\tablero ((fila, columna), valor) -> actualizarTabla tablero (fila, columna) valor) plantilla (zip posicionesVacias numerosAleatorios)
    return tableroConNumeros


-- | Imprime el tablero de Sudoku en la consola.
imprimirTablero :: Board -> IO ()
imprimirTablero = mapM_ (putStrLn . intercalate " " . map mostrarCelda)

-- | Convierte un número entero en una cadena de caracteres para mostrarlo en el tablero.
mostrarCelda :: Int -> String
mostrarCelda 0 = "_"
mostrarCelda n = show n

-- | Verifica si una posición en el tablero de Sudoku es válida.
esPosicionValida :: Position -> Bool
esPosicionValida (fila, columna) = fila >= 1 && fila <= 9 && columna >= 1 && columna <= 9


-- | Maneja una entrada de usuario inválida.
manejarEntradaInvalida :: IO ()
manejarEntradaInvalida = putStrLn "Entrada inválida. Por favor, ingrese números del 1 al 9."

-- | Función principal que maneja el juego de Sudoku.
jugar :: Board -> IO ()
jugar board = do
    putStrLn "Introduce una posición (fila columna valor) o 'exit' para salir: "
    input <- getLine
    if input == "exit" then
        putStrLn "Hasta luego :)"
    else do
        let [fila, columna, valor] = map read $ words input :: [Int]
        if esPosicionValida (fila, columna) && valor >= 1 && valor <= 9 then do
            let nuevaTabla = actualizarTabla board (fila, columna) valor
            if esResuelto nuevaTabla then
                putStrLn "¡Felicidades! ¡Has resuelto el Sodoku! :)"
            else do
                putStrLn "Tablero actualizado: "
                imprimirTablero nuevaTabla
                jugar nuevaTabla
        else do
            manejarEntradaInvalida
            jugar board

-- | Actualiza el tablero de Sudoku con un nuevo valor en una posición dada.
actualizarTabla :: Board -> Position -> Int -> Board
actualizarTabla tablero (fila, columna) valor =
    if fila < 1 || fila > 9 || columna < 1 || columna > 9 || valor < 1 || valor > 9
        then
            tablero
        else
            let (arriba, filaActual:abajo) = splitAt (fila - 1) tablero
                filaModificada = actualizarFila filaActual (columna - 1) valor
            in arriba ++ [filaModificada] ++ abajo

-- | Actualiza una fila del tablero de Sudoku con un nuevo valor en la posición especificada.
actualizarFila :: [Int] -> Int -> Int -> [Int]
actualizarFila fila columna valor =
    let (izquierda, _:derecha) = splitAt columna fila
    in izquierda ++ [valor] ++ derecha


-- | Verifica si un tablero de Sudoku está completamente resuelto.
esResuelto :: Board -> Bool
esResuelto tablero = 
    all (all (/= 0)) tablero && -- Verifica si todas las celdas están llenas 
    all sinRepeticiones (tablero ++ transpose tablero) && -- Verifica filas y columnas
    all sinRepeticiones (bloques tablero) -- Verifica bloques de 3x3

-- | Verifica si una lista de enteros no contiene repeticiones.
sinRepeticiones :: [Int] -> Bool
sinRepeticiones xs = nub xs == xs

-- | Divide una lista en sublistas de tamaño dado.
chunksOf :: Int -> [a] -> [[a]] 
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Divide un tablero de Sudoku en bloques de 3x3.
bloques :: Board -> [[Int]]
bloques tablero = concatMap (agruparBloques . map (chunksOf 3)) (chunksOf 3 tablero)
    where
        agruparBloques bloquesFilas = map concat (transpose bloquesFilas)

