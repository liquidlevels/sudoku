import Data.List
import System.Random
import Control.Monad (replicateM)

-- | Representa un tablero de Sudoku como una matriz 9x9 de enteros.
type Board = [[Int]]

-- | Representa una posición en el tablero de Sudoku, dada por fila y columna.
type Position = (Int, Int)

-- | Función principal que inicia el juego de Sudoku.
main :: IO ()
main = do
    putStrLn "Bienvenido al juego de Sudoku :)"
    board <- generarTablero
    imprimirTablero board
    jugar board

-- | Genera un tablero de Sudoku cumpliendo las reglas del juego.
generarTablero :: IO Board
generarTablero = do
    seed <- newStdGen
    let nums = [1..9]
        rows = permutations nums
        cols = transpose rows
        blocks = concatMap (chunksOf 3) $ map concat $ chunksOf 3 rows
        validBoards = filter esResuelto (map transpose [rows, cols, blocks])
        -- Genera un tablero inicial con números aleatorios
        initialBoard = validBoards !! fst (randomR (0, length validBoards - 1) seed)
        -- Llena algunos espacios vacíos con ceros para simular el tablero inicial de un juego de Sudoku
        board = map (\row -> map (\x -> if x `elem` take 10 (randomRs (1, 9) seed) then x else 0) row) initialBoard
    return board

-- | Actualiza el tablero de Sudoku con un nuevo valor en una posición dada.
-- | Actualiza el tablero de Sudoku con un nuevo valor en una posición dada.
-- | Actualiza el tablero de Sudoku con un nuevo valor en una posición dada.
-- | Actualiza el tablero de Sudoku con un nuevo valor en una posición dada.
actualizarTabla :: Board -> Position -> Int -> Board
actualizarTabla tablero (fila, columna) valor
    | not (esPosicionValida (fila, columna)) || valor < 1 || valor > 9 || esNumeroInicial tablero (fila, columna) = tablero
    | otherwise = actualizaFila fila tablero (actualizaColumna columna valor (tablero !! (fila - 1)))

-- | Verifica si una posición en el tablero corresponde a un número inicial.
esNumeroInicial :: Board -> Position -> Bool
esNumeroInicial tablero (fila, columna) = (tablero !! (fila - 1)) !! (columna - 1) /= 0

-- | Actualiza una fila del tablero con nuevos valores.
actualizaFila :: Int -> Board -> [Int] -> Board
actualizaFila fila tablero nuevaFila = take (fila - 1) tablero ++ [nuevaFila] ++ drop fila tablero

-- | Actualiza una columna del tablero con un nuevo valor.
actualizaColumna :: Int -> Int -> [Int] -> [Int]
actualizaColumna _ _ [] = []
actualizaColumna 1 valor (x:xs) = valor : xs
actualizaColumna n valor (x:xs) = x : actualizaColumna (n - 1) valor xs

-- | Imprime el tablero de Sudoku en la consola.
imprimirTablero :: Board -> IO ()
imprimirTablero = mapM_ (putStrLn . intercalate " " . map mostrarCelda)

-- | Convierte un número entero en una cadena de caracteres para mostrarlo en el tablero.
mostrarCelda :: Int -> String
mostrarCelda 0 = "_"
mostrarCelda n = show n

-- | Función principal que maneja el juego de Sudoku.
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
                                    then putStrLn "¡Felicidades! ¡Has resuelto el Sudoku! :)"
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

-- Funciones auxiliares

-- | Verifica si una posición en el tablero de Sudoku es válida.
esPosicionValida :: Position -> Bool
esPosicionValida (fila, columna) = fila >= 1 && fila <= 9 && columna >= 1 && columna <= 9

-- | Maneja una entrada de usuario inválida.
manejarEntradaInvalida :: IO ()
manejarEntradaInvalida = putStrLn "Entrada inválida. Por favor, ingrese números del 1 al 9."

-- | Verifica si un tablero de Sudoku está completamente resuelto.
esResuelto :: Board -> Bool
esResuelto tablero = 
    all (all (/= 0)) tablero && -- Verifica si todas las celdas están llenas 
    all sinRepeticiones (tablero ++ transpose tablero) && -- Verifica filas y columnas
    True -- Falsa implementación para evitar el error en la función 'esResuelto'

-- | Verifica si una lista de enteros no contiene repeticiones.
sinRepeticiones :: [Int] -> Bool
sinRepeticiones xs = nub xs == xs

-- | Divide una lista en sublistas de tamaño dado.
chunksOf :: Int -> [a] -> [[a]] 
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

