import Data.List
import System.Random

--definicion de tipos 
type Board = [[Int]]
type Position = (Int, Int)

--Funcion principal que inicia el juego
main :: IO ()
main = do
    putStrLn "Bienvenido al juego de sodoku :) "
    board <- generarTablero
    imprimirTablero board
    jugar board

-- Genera un tablero de sodoku valido
generarTablero :: IO Board
generarTablero = do 
    seed <- newStdGen
    let valores = take 81 $ randomRs (1, 9) seed :: [Int]
    return $ chunksOf 9 valores

--imprime el tablero
imprimirTablero :: Board -> IO ()
imprimirTablero = mapM_ (putStrLn . intercale " " . map show) 

--funcion que maneja el juego
jugar :: Board -> IO()
jugar board = do 
    putStrLn "Introduce una posicion (fila columna valor) o 'exit' para salir: "
    input <- getLine
    if input == "exit" then
        putStrLn "Hasta luego :)"
    else do
        let [fila, columna, valor] = map read $ words input :: [Int]
            nuevaTabla = actualizarTabla board (fila, columna) valor
        if esResuelto nuevaTabla then
            putStrLn "Felicidades! Has resuelto el Sodoku! :)"
        else do
            putStrLn "Tablero actualizado: "
            imprimirTablero nuevaTabla
            jugar nuevaTabla

--actualiza el valor en una posicion dada en el tablero
actualizarTabla :: Board -> Position -> Int -> Board
actualizarTabla tablero (fila, columna) valor =
    let (arriba, filaActual:abajo) = splitAt columna filaActual
        (izquierda, _:derecha) = splitAt columna filaActual
    in arriba ++ [izquierda ++ valor:derecha] ++ abajo

--verificar si esta resuleto
esResuelto :: Board -> Bool
esResuelto tablero = 
    all (all (/= 0)) tablero && -- verifica si todas las celdas estan llenas 
    all sinRepeticiones (tablero ++ transpose tablero) && --verifica filas y columnas
    all sinRepeticiones (bloques tablero) --verifica bloques de 3x3

--verifica si no hay repeticones en una fila, columna o bloque
sinRepeticiones :: [Int] -> Bool
sinRepeticiones xs = nub xs == xs

--divide una lista en sublistas de tama;o dado
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

--obtiene los bloques de 3x3
bloques :: Board -> [Board]
bloques = concarMap (chunksOf 3) . map transpose . chunksOf 3
