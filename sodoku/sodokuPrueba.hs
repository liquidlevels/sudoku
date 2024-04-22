import Data.List
import System.Random
import Control.Monad (replicateM)

type Board = [[Int]]
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

main :: IO ()
main = do
    putStrLn "Bienvenido al juego de Sodoku :) "
    board <- generarTablero plantillaInicial
    imprimirTablero board
    jugar board

generarTablero :: Board -> IO Board
generarTablero plantilla = do 
    seed <- newStdGen
    let posicionesVacias = filterPosicionesVacias plantilla
        numerosAleatorios = take (length posicionesVacias) $ randomRs (1, 9) seed :: [Int]
        tableroConNumeros = foldr (\(pos, val) tab -> actualizarTabla tab pos val) plantilla (zip posicionesVacias numerosAleatorios)
    return tableroConNumeros

imprimirTablero :: Board -> IO ()
imprimirTablero = mapM_ (putStrLn . intercalate " " . map mostrarCelda)

mostrarCelda :: Int -> String
mostrarCelda 0 = "_"
mostrarCelda n = show n

esPosicionValida :: Position -> Bool
esPosicionValida (fila, columna) = fila >= 1 && fila <= 9 && columna >= 1 && columna <= 9

manejarEntradaInvalida :: IO ()
manejarEntradaInvalida = putStrLn "Entrada inválida. Por favor, ingrese números del 1 al 9."

jugar :: Board -> IO ()
jugar board = loop board
    where
        loop :: Board -> IO ()
        loop board = do
            putStrLn "Introduce una posición (fila columna valor) o 'exit' para salir: "
            input <- getLine
            case input of
                "exit" -> putStrLn "Hasta luego :)"
                _ -> case parseInput input of
                        Just (fila, columna, valor) ->
                            if esPosicionValida (fila, columna) && valor >= 1 && valor <= 9
                            then do
                                let nuevaTabla = actualizarTabla board (fila, columna) valor
                                if esResuelto nuevaTabla
                                    then putStrLn "¡Felicidades! ¡Has resuelto el Sodoku! :)"
                                    else do
                                        putStrLn "Tablero actualizado: "
                                        imprimirTablero nuevaTabla
                                        loop nuevaTabla
                            else do
                                manejarEntradaInvalida
                                loop board
                        Nothing -> do
                            manejarEntradaInvalida
                            loop board

        parseInput :: String -> Maybe (Int, Int, Int)
        parseInput input =
            case map read $ words input of
                [fila, columna, valor] -> Just (fila, columna, valor)
                _ -> Nothing


actualizarTabla :: Board -> Position -> Int -> Board
actualizarTabla tablero (fila, columna) valor
    | fila < 1 || fila > 9 || columna < 1 || columna > 9 || valor < 1 || valor > 9 = tablero
    | otherwise = actualizaFila fila tablero (actualizaColumna columna valor (tablero !! (fila - 1)))

actualizaColumna :: Int -> Int -> [Int] -> [Int]
actualizaColumna _ _ [] = []
actualizaColumna 1 valor (_:xs) = valor:xs
actualizaColumna n valor (x:xs) = x : actualizaColumna (n-1) valor xs

actualizaFila :: Int -> Board -> [Int] -> Board
actualizaFila _ [] _ = []
actualizaFila 1 (_:xs) fila = fila : xs
actualizaFila n (x:xs) fila = x : actualizaFila (n-1) xs fila

esResuelto :: Board -> Bool
esResuelto tablero = 
    all (all (/= 0)) tablero && -- Verifica si todas las celdas están llenas 
    all sinRepeticiones (tablero ++ transpose tablero) && -- Verifica filas y columnas
    all sinRepeticiones (bloques tablero) -- Verifica bloques de 3x3

sinRepeticiones :: [Int] -> Bool
sinRepeticiones xs = nub xs == xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

bloques :: Board -> [[Int]]
bloques tablero = concatMap (agruparBloques . map (chunksOf 3)) (chunksOf 3 tablero)
    where
        agruparBloques bloquesFilas = map concat (transpose bloquesFilas)

filterPosicionesVacias :: Board -> [Position]
filterPosicionesVacias tablero = [(fila, columna) | fila <- [1..9], columna <- [1..9], (tablero !! (fila - 1)) !! (columna - 1) == 0]

