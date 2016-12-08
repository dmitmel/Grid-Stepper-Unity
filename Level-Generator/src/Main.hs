module Main where

import LevelGenerator.Generator
import LevelGenerator.Level
import System.Environment (getArgs)
import System.Random (getStdGen, setStdGen, mkStdGen)
import Graphics.Proc
import System.Exit (exitSuccess)
import Control.Monad (when)

main :: IO ()
main = runProc $ def { procSetup = setup, procDraw = draw, procKeyPressed = keyPressed }

cubeSize :: Float
cubeSize = 30.0
cubesOffset :: Float
cubesOffset = cubeSize / 2
levelStrokeWidth :: Float
levelStrokeWidth = 3.0
cubeStrokeWidth :: Float
cubeStrokeWidth = 1.0

setup :: Pio Level
setup = do
    size (800, 600)
    liftIO $ createLevel True

createLevel :: Bool -> IO Level
createLevel firstLaunch = do
    args <- getArgs
    when (firstLaunch && not (null args)) $ setStdGen $ mkStdGen $ read $ head args
    gen <- getStdGen
    let seed = read (head $ words $ show gen) - 1 :: Int
    putStrLn $ "Seed: " ++ show seed
    let (level, newGen) = randomLevel gen
    setStdGen newGen
    return level

draw :: Level -> Draw
draw level = do
    background (grey 255)

    stroke black
    noFill
    strokeWeight levelStrokeWidth
    let floatLevelWidth  = fromIntegral $ levelWidth level  :: Float
        floatLevelHeight = fromIntegral $ levelHeight level :: Float
    rect (cubesOffset, cubesOffset) (floatLevelWidth * cubeSize, floatLevelHeight * cubeSize)
    strokeWeight cubeStrokeWidth

    let levelWithCoords = zip [0.0..] $ map (zip [0.0..]) level
    forM_ levelWithCoords (\(y, row) -> forM_ row (\(x, cube) -> drawCube (x, y) cube))

drawCube :: P2 -> Cube -> Draw
drawCube (x, y) cube = do
    case cube of
        Cube -> do
            fill (grey 200)
            stroke (grey 0)
        NoCube -> do
            noFill
            noStroke
        Start -> do
            fill (rgb 0 200 0)
            stroke (grey 0)
        Finish -> do
            fill (grey 0)
            stroke (grey 0)
    rect (cubesOffset + x * cubeSize, cubesOffset + y * cubeSize) (cubeSize, cubeSize)

keyPressed :: Update Level
keyPressed level = do
    pressedKey <- key
    case pressedKey of
        Graphics.Proc.Char c -> case c of
            'q' -> liftIO exitSuccess
            'r' -> liftIO $ createLevel False
            _ -> return level
        _ -> return level
