module Main where

import           Control.Concurrent       (threadDelay)
import           Graphics.Proc
import           LevelGenerator.Generator
import           LevelGenerator.Level
import           System.Environment       (getArgs)
import           System.Exit              (exitSuccess)
import           System.Random            (StdGen, getStdGen, mkStdGen, randomR,
                                           setStdGen)

main :: IO ()
main = runProc $ def { procSetup = setup
                     , procDraw = draw
                     , procKeyPressed = keyPressed
                     , procUpdate = update
                     }

type Path = ((Int, Int), (Int, Int))

setup :: Pio (Level, CubeCoord, StdGen, [Path])
setup = do
    size (800, 600)
    liftIO $ createLevel True

createLevel :: Bool -> IO (Level, CubeCoord, StdGen, [Path])
createLevel firstLaunch = do
    args <- getArgs
    when (firstLaunch && not (null args)) $ setStdGen $ mkStdGen $ read $ head args
    gen <- getStdGen
    let seed = read (head $ words $ show gen) - 1 :: Int
    putStrLn $ "Seed: " ++ show seed
    let (level, cubeCoord, newGen) = randomEmptyLevel gen
    setStdGen newGen
    return (level, cubeCoord, newGen, [])

cubeSize :: Float
cubeSize = 30.0
cubesOffset :: Float
cubesOffset = cubeSize / 2
secondsBetweenSteps :: Float
secondsBetweenSteps = 0.025
levelStrokeWidth :: Float
levelStrokeWidth = 3.0
cubeStrokeWidth :: Float
cubeStrokeWidth = 1.0
pathStrokeWidth :: Float
pathStrokeWidth = 2.5

draw :: (Level, CubeCoord, StdGen, [Path]) -> Draw
draw (level, _, _, paths) = do
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

    forM_ paths drawPath

drawCube :: P2 -> Cube -> Draw
drawCube (x, y) cube = do
    case cube of
        Cube -> do
            fill gray
            stroke black
        NoCube -> do
            noFill
            noStroke
        Start -> do
            fill green
            stroke (grey 0)
        Finish -> do
            fill black
            stroke black
    rect (cubesOffset + x * cubeSize, cubesOffset + y * cubeSize) (cubeSize, cubeSize)

drawPath :: Path -> Draw
drawPath (startCoord, endCoord) = do
    let (x1, y1) = startCoord
        (x2, y2) = endCoord
        floatX1 = fromIntegral x1 :: Float
        floatY1 = fromIntegral y1 :: Float
        floatX2 = fromIntegral x2 :: Float
        floatY2 = fromIntegral y2 :: Float
    stroke black
    strokeWeight pathStrokeWidth
    line (cubesOffset + floatX1 * cubeSize + cubeSize / 2.0, cubesOffset + floatY1 * cubeSize + cubeSize / 2.0)
         (cubesOffset + floatX2 * cubeSize + cubeSize / 2.0, cubesOffset + floatY2 * cubeSize + cubeSize / 2.0)

keyPressed :: Update (Level, CubeCoord, StdGen, [Path])
keyPressed currentData = do
    pressedKey <- key
    case pressedKey of
        Graphics.Proc.Char c -> case c of
            'q' -> liftIO exitSuccess
            'r' -> liftIO $ createLevel True
            _   -> return currentData
        _ -> return currentData

data Direction = DUp | DRight | DDown | DLeft deriving (Show, Eq)

update :: Update (Level, CubeCoord, StdGen, [Path])
update (level, cubeCoord, gen, paths) =
    if cubeAt cubeCoord level == Finish then
        return (level, cubeCoord, gen, paths)
    else do
        liftIO $ threadDelay $ round $ secondsBetweenSteps * 1000000

        let (x, y) = cubeCoord

        let upCube    = cubeAt (x, y - 1) level
            rightCube = cubeAt (x + 1, y) level
            downCube  = cubeAt (x, y + 1) level
            leftCube  = cubeAt (x - 1, y) level

        let hasDirectionUp      = y - 1 >= 0                && upCube    == NoCube
            hasDirectionRight   = x + 1 < levelWidth level  && rightCube == NoCube
            hasDirectionDown    = y + 1 < levelHeight level && downCube  == NoCube
            hasDirectionLeft    = x - 1 >= 0                && leftCube  == NoCube
            availableDirections = joinMaybes [ if hasDirectionUp    then Just DUp    else Nothing
                                             , if hasDirectionRight then Just DRight else Nothing
                                             , if hasDirectionDown  then Just DDown  else Nothing
                                             , if hasDirectionLeft  then Just DLeft  else Nothing
                                             ]
        let (selectedDirectionI, resultGen) = randomR (0, length availableDirections - 1) gen
            selectedDirection               = availableDirections !! selectedDirectionI

        let newCubeCoord = case selectedDirection of
                DUp    -> (x, y - 1)
                DRight -> (x + 1, y)
                DDown  -> (x, y + 1)
                DLeft  -> (x - 1, y)

        if null availableDirections
            then return (level # ((x, y), Finish), cubeCoord, resultGen, paths)
            else return (level # (newCubeCoord, Cube), newCubeCoord, resultGen, (cubeCoord, newCubeCoord):paths)

joinMaybes :: Eq a => [Maybe a] -> [a]
joinMaybes = map (\(Just x) -> x) . filter (/= Nothing)
