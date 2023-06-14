module Treasure where

import Data.Array
import System.Random

import Game

generateMazeTreasure :: RandomGen g => PlayField -> g -> (Treasure, g) 
generateMazeTreasure playField gen =
    let ((minRow, minCol), (maxRow, maxCol)) = bounds playField
        (x, gen') = randomR (minRow, maxRow) gen
        (y, gen'') = randomR (minCol, maxCol) gen'
        mazeSlot = playField ! (x,y)
    in if(mazeSlot == MazePath)
        then (Treasure { tPosition = (x,y), tCharacter = '*'}, gen'')
        else generateMazeTreasure playField gen''

generateNMazeTreasures :: RandomGen g => Int -> PlayField -> g -> ([Treasure], g)
generateNMazeTreasures 0 playField gen = ([], gen)
generateNMazeTreasures n playField gen =
  let (treasure, newGen) = generateMazeTreasure playField gen
      (treasures, newGen') = generateNMazeTreasures (n - 1) playField newGen
  in (treasure : treasures, newGen')

generateMazeTreasures :: RandomGen g => PlayField -> g -> ([Treasure], g)
generateMazeTreasures playField gen =
    generateNMazeTreasures 20 playField gen

checkTreasureCollection :: PacManGame -> PacManGame
checkTreasureCollection game =
    let treasures = gTreasures game
        playerPosition = pPosition (gPlayer game)
        playerOnTreasure = filter (\x -> (tPosition x) == playerPosition) treasures
    in if (length playerOnTreasure) > 0
        then
            let treasure = head playerOnTreasure
                game' = removeTreasure game treasure
            in addScore game'
        else game

addScore :: PacManGame -> PacManGame
addScore game = 
    let score = gScore game
        score' = score + 10
    in game { gScore = score'}

removeTreasure :: PacManGame -> Treasure -> PacManGame
removeTreasure game treasure = 
    let treasures = gTreasures game
        treasures' = filter (\t -> t /= treasure) treasures
    in game {gTreasures = treasures'}