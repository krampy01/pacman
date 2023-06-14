module Monster where

import Data.Array
import System.Random

import Game
import Maze
import Player



isMazeCrossroad :: (Int, Int) -> PlayField -> Bool
isMazeCrossroad position playField = 
    let slots = getSurroundingMazeSlots position playField
        mazePathSlotsCount = length $ filter (\(mazeSlot, _) -> mazeSlot == MazePath) slots 
    in mazePathSlotsCount >= 3

isMonsterHeadingInToWall :: Monster -> PlayField -> Bool
isMonsterHeadingInToWall monster playField =
    let (dx, dy) = mDirection monster
        (px, py) = mPosition monster
        nextPosition = (dx+px, dy+py)
        nextSlot = playField ! nextPosition
    in nextSlot == MazeWall

--could get exception on empty list but for now we don't use it on empty lists
pickRandomFromList :: RandomGen g => [a] -> g -> (a, g)
pickRandomFromList xs gen = ((xs !! index), gen')
  where
    (index, gen') = randomR (0, length xs - 1) gen

getNewMonsterDirection :: RandomGen g => Monster -> PlayField -> g -> (Monster, g)
getNewMonsterDirection monster playField gen = 
    let (mx, my) = mPosition monster
        slots = getSurroundingMazeSlots (mx, my) playField
        mazePathSlots = filter (\(mazeSlot, _) -> mazeSlot == MazePath) slots 
        ((_, (x, y)), gen') = pickRandomFromList mazePathSlots gen
        monster' = monster { mDirection = ((x - mx), (y - my))}
    in (monster', gen')

isMonsterNotMoving :: Monster -> Bool
isMonsterNotMoving monster = 
    (0, 0) == mDirection monster        

checkMonsterDirection :: RandomGen g => Monster -> PlayField -> g -> (Monster, g)
checkMonsterDirection monster playField gen =
    let isCrossroad = isMazeCrossroad (mPosition monster) playField
        isHeadingInToWall = isMonsterHeadingInToWall monster playField
        isNotMoving = isMonsterNotMoving monster
    in if isCrossroad || isHeadingInToWall || isNotMoving
        then getNewMonsterDirection monster playField gen
        else (monster, gen)

moveMonster :: RandomGen g => Monster -> PlayField -> g -> (Monster, g)
moveMonster monster playField gen = 
    let (monster', gen') = checkMonsterDirection monster playField gen
        (dx, dy) = mDirection monster'
        monster'' = moveGameCharacter (\(x, y) -> (x + dx, y + dy)) monster' playField
    in (monster'', gen')

moveMonsters :: RandomGen g => [Monster] -> PlayField -> g -> ([Monster], g)
moveMonsters monsters playField gen = foldl moveMonsterAndUpdateGen ([], gen) monsters
  where
    moveMonsterAndUpdateGen (movedMonsters, gen') monster = 
      let (newMonster, gen'') = moveMonster monster playField gen'
      in (movedMonsters ++ [newMonster], gen'')
