module Player where

import Data.Array
import System.Random

import Game

updatePlayerPosition :: Maybe Char -> Player -> PlayField-> Player
updatePlayerPosition key player playField =
    case key of
        Just 'w' -> moveGameCharacterUp player playField
        Just 's' -> moveGameCharacterDown player playField
        Just 'd' -> moveGameCharacterRight player playField
        Just 'a' -> moveGameCharacterLeft player playField
        _ -> player

moveGameCharacter :: GameCharacter a => ((Int, Int) -> (Int, Int)) -> a -> PlayField -> a
moveGameCharacter f character playField =
    let position = getPosition character
        newPosition = f position
        fieldPosition = playField ! newPosition
    in
        if fieldPosition == MazeWall
            then character
            else setPosition character newPosition

moveGameCharacterUp:: GameCharacter a => a -> PlayField -> a 
moveGameCharacterUp player playField =
    moveGameCharacter (\(x, y) -> (x - 1, y)) player playField

moveGameCharacterDown:: GameCharacter a => a -> PlayField -> a
moveGameCharacterDown player playField =
    moveGameCharacter (\(x, y) -> (x + 1, y)) player playField

moveGameCharacterLeft:: GameCharacter a => a -> PlayField -> a
moveGameCharacterLeft player playField =
    moveGameCharacter (\(x, y) -> (x, y - 1)) player playField

moveGameCharacterRight:: GameCharacter a => a -> PlayField -> a
moveGameCharacterRight player playField =
    moveGameCharacter (\(x, y) -> (x, y + 1)) player playField
