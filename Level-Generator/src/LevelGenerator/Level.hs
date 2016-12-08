module LevelGenerator.Level
( CubeCoord
, Cube(..)
, Level, levelWidth, levelHeight, cubeAt, printLevel, (#)
, (!!=)
) where

data Cube = NoCube | Cube | Start | Finish deriving (Show, Eq)
type CubeCoord = (Int, Int)
type Level = [[Cube]]

levelWidth :: Level -> Int
levelWidth = length . head

levelHeight :: Level -> Int
levelHeight = length

(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (n, x) = take n xs ++ [x] ++ drop (n + 1) xs

(#) :: Level -> (CubeCoord, Cube) -> Level
level # (cubeCoord, cube)
    | x < 0                  = error $ "Too small x (" ++ show x ++ ")"
    | y < 0                  = error $ "Too small y (" ++ show y ++ ")"
    | x >= levelWidth level  = error $ "Too big x (" ++ show x ++ ")"
    | y >= levelHeight level = error $ "Too big y (" ++ show y ++ ")"
    | otherwise              = level !!= (y, level !! y !!= (x, cube))
    where (x, y) = cubeCoord

cubeAt :: (Int, Int) -> Level -> Cube
cubeAt (x, y) level = level !! y !! x

printLevel :: Level -> IO ()
printLevel level = do
    putStrLn $ "width: " ++ show (levelWidth level) ++ ", height: " ++ show (levelHeight level)
    putStrLn $ " " ++ duplicate (levelWidth level) "_" ++ " "
    mapM_ (\row -> do
        putStr "|"
        mapM_ (putStr . showCube_) row
        putStrLn "|") (init level)
    putStr "|"
    mapM_ (putStr . showLastCube_) $ last level
    putStrLn "|\n"

showCube_ :: Cube -> String
showCube_ NoCube = " "
showCube_ Cube   = "x"
showCube_ Start  = "s"
showCube_ Finish = "f"

showLastCube_ :: Cube -> String
showLastCube_ NoCube = "_"
showLastCube_ Cube   = "x"
showLastCube_ Start  = "s"
showLastCube_ Finish = "f"

duplicate :: Int -> [a] -> [a]
duplicate n xs = concat $ replicate n xs
