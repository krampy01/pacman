module Game where

import Data.Array
import System.Random

data MazeSlot = MazeWall | MazePath deriving Eq
type PlayField =  Array (Int, Int) MazeSlot

data PacManGame = PacManGame
    {  
         gPlayField  :: PlayField
        ,gPlayer     :: Player
        ,gScore      :: Int
        ,gFinished   :: Bool
        ,gMonsters   :: [Monster]
        ,gTreasures  :: [Treasure]
        ,gRandomGen  :: StdGen
    }

class GameCharacter a where
    getPosition :: a -> (Int, Int)
    setPosition :: a -> (Int, Int) -> a
    getCharacter :: a -> Char

data Player = Player
    {
         pPosition :: (Int, Int)
        ,pCharacter :: Char
    }

data Monster = Monster
    { 
         mPosition  :: (Int, Int)
        ,mDirection :: (Int, Int)
        ,mCharacter :: Char
    }

data Treasure = Treasure 
    {
         tPosition :: (Int, Int)
        ,tCharacter :: Char
    } deriving Eq

instance GameCharacter Player where
    getPosition = pPosition
    setPosition player newPosition = player { pPosition = newPosition }
    getCharacter = pCharacter

instance GameCharacter Monster where
    getPosition = mPosition
    setPosition monster newPosition = monster { mPosition = newPosition }
    getCharacter = mCharacter