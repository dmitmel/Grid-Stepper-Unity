module LevelGenerator.Generator(randomLevel, randomEmptyLevel) where

import           LevelGenerator.Level
import           System.Random        (RandomGen, randomR)

randomLevel :: RandomGen g => g -> (Level, g)
randomLevel gen = botModify botModifyGen emptyLevel startCoord
    where (emptyLevel, startCoord, botModifyGen) = randomEmptyLevel gen

data Direction = DUp | DRight | DDown | DLeft deriving (Show, Eq)

joinMaybes :: Eq a => [Maybe a] -> [a]
joinMaybes = map (\(Just x) -> x) . filter (/= Nothing)

botModify :: RandomGen g => g -> Level -> CubeCoord -> (Level, g)
botModify gen level (x, y) = if null availableDirections then (putFinishHere, resultGen) else doStep
    where upCube    = cubeAt (x, y - 1) level
          rightCube = cubeAt (x + 1, y) level
          downCube  = cubeAt (x, y + 1) level
          leftCube  = cubeAt (x - 1, y) level

          hasDirectionUp      = y - 1 >= 0                && upCube    == NoCube
          hasDirectionRight   = x + 1 < levelWidth level  && rightCube == NoCube
          hasDirectionDown    = y + 1 < levelHeight level && downCube  == NoCube
          hasDirectionLeft    = x - 1 >= 0                && leftCube  == NoCube
          availableDirections = joinMaybes [ if hasDirectionUp    then Just DUp    else Nothing
                                           , if hasDirectionRight then Just DRight else Nothing
                                           , if hasDirectionDown  then Just DDown  else Nothing
                                           , if hasDirectionLeft  then Just DLeft  else Nothing
                                           ]
          (selectedDirectionI, resultGen) = randomR (0, length availableDirections - 1) gen
          selectedDirection               = availableDirections !! selectedDirectionI

          putFinishHere = level # ((x, y), Finish)
          newCubeCoord = case selectedDirection of
              DUp    -> (x, y - 1)
              DRight -> (x + 1, y)
              DDown  -> (x, y + 1)
              DLeft  -> (x - 1, y)
          doStep = botModify resultGen (level # (newCubeCoord, Cube)) newCubeCoord

randomEmptyLevel :: RandomGen g => g -> (Level, CubeCoord, g)
randomEmptyLevel widthGen = (emptyLevelWithStart, (startCubeX, startCubeY), lastGen)
    where (width, heightGen)           = randomR (2, 20) widthGen
          (height, startCubeXGen)      = randomR (2, 20) heightGen
          (startCubeX, startCubeYGen)  = randomR (0, width - 1) startCubeXGen
          (startCubeY, lastGen)        = randomR (0, height - 1) startCubeYGen
          emptyLevel                   = replicate height $ replicate width NoCube
          emptyLevelWithStart          = emptyLevel # ((startCubeX, startCubeY), Start)
