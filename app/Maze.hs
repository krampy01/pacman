module Maze where

import Data.Array
import System.Random

import Game


generateMaze :: RandomGen g => (Int,Int) -> g -> (PlayField, g)
generateMaze (x, y) gen =
     let playField = array ((0, 0), (x-1, y-1)) [ ((i, j), generatePlayFieldFence i j x y) | i <- [0..x-1], j <- [0..y-1] ]
     in generateMazeSlotsLoop playField gen

isMazeOuterRim :: (Int, Int) -> PlayField -> Bool
isMazeOuterRim (x, y) playField =
    let ((minRow, minCol), (maxRow, maxCol)) = bounds playField
    in ((x-2 < minRow) || (y-2 < minCol) || (x+2 > maxRow) || (y+2 > maxCol))

generateMazeSlot :: RandomGen g => (Int, Int) -> PlayField -> g -> (PlayField, g)
generateMazeSlot (x, y) playField gen = 
    let isOuterRim = isMazeOuterRim (x, y) playField
        (randomNumber, gen') = randomR (0, 100) gen        
        playField' = playField // [((x, y), MazeWall)]
        arePathsReachable = areAllPathsReachable playField'
    in if isOuterRim || (randomNumber > (10 :: Int))  || not arePathsReachable    
        then (playField, gen')
        else (playField', gen')


generateMazeSlotsLoop :: RandomGen g => PlayField -> g -> (PlayField, g)
generateMazeSlotsLoop playField gen =
  generateMazeSlots 20 playField gen

generateMazeSlots :: RandomGen g => Int -> PlayField -> g -> (PlayField, g)
generateMazeSlots 0 playField gen = (playField, gen)
generateMazeSlots n playField gen =
  let (updatedField, newGen) = generateMazeSlotsOnce playField gen
  in generateMazeSlots (n - 1) updatedField newGen

generateMazeSlotsOnce :: RandomGen g => PlayField -> g -> (PlayField, g)
generateMazeSlotsOnce playField gen = generateMazeSlots' (assocs playField) playField gen
  where
    generateMazeSlots' [] playField gen = (playField, gen)
    generateMazeSlots' ((position, _):rest) playField gen =
      let (updatedPlayField, newGen) = generateMazeSlot position playField gen
      in generateMazeSlots' rest updatedPlayField newGen
    
generatePlayFieldFence :: Int -> Int -> Int -> Int -> MazeSlot
generatePlayFieldFence i j x y
 | i == 0 || i == x-1 = MazeWall
 | j == 0 || j == y-1 = MazeWall
 | otherwise = MazePath

areAllPathsReachable :: PlayField -> Bool
areAllPathsReachable playField =
    all (\(position, field) -> hasAtLeastTwoPathSlotsMembers position playField) (assocs playField)

hasAtLeastTwoPathSlotsMembers :: (Int, Int) -> PlayField -> Bool
hasAtLeastTwoPathSlotsMembers (x, y) playField =
    let isOuterRim = isMazeOuterRim (x, y) playField
        slots = getSurroundingMazeSlots (x, y) playField
        isWall = (playField ! (x, y)) == MazeWall
        mazePathSlotsCount = length $ filter (\(mazeSlot, _) -> mazeSlot == MazePath) slots
    in isOuterRim || isWall || (mazePathSlotsCount >= 2)

getSurroundingMazeSlots :: (Int, Int) -> PlayField -> [(MazeSlot, (Int, Int))]
getSurroundingMazeSlots (x, y) playField = 
    let topSlot = getPlayFieldSlotWithPosition playField (x-1, y)
        bottomSlot = getPlayFieldSlotWithPosition playField (x+1, y)
        leftSlot = getPlayFieldSlotWithPosition playField (x, y-1)
        leftRight = getPlayFieldSlotWithPosition playField (x, y+1)
    in [topSlot, bottomSlot, leftSlot, leftRight]

getPlayFieldSlotWithPosition :: PlayField -> (Int, Int) -> (MazeSlot, (Int, Int))
getPlayFieldSlotWithPosition playField position = 
    (playField ! position, position) 