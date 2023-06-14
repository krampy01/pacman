module GameLoop where

--import Data.Foldable
import Control.Concurrent (threadDelay)
import System.Random
import System.IO

import Game
import Rendering
import Maze
import Monster
import Player
import Treasure

gameLoopDelay :: IO ()
gameLoopDelay = threadDelay (1 * 10 ^ 5)

initGame :: (Int, Int) -> Int -> PacManGame
initGame (x, y) seed = 
    let gen = mkStdGen seed
        (playField, gen') = generateMaze (x, y) gen
        (treasures, gen'') = generateMazeTreasures playField gen'
        player = Player { pPosition = (1,1), pCharacter = 'o'}
        monster1 = Monster { mPosition = (1, y-2), mCharacter = '@', mDirection = (0,0)}
        monster2 = Monster { mPosition = (x-2, 1), mCharacter = '#', mDirection = (0,0)}
        monster3 = Monster { mPosition = (x-2, y-2), mCharacter = '&', mDirection = (0,0)}
        monsters = [monster1, monster2, monster3]
        
        score = 0
    in PacManGame { 
        gPlayField = playField, 
        gPlayer = player,
        gScore = score,
        gFinished = False,
        gMonsters = monsters,
        gRandomGen = gen'',
        gTreasures = treasures }

gameLoop :: PacManGame -> IO (Either String PacManGame)
gameLoop game = do
    renderGame game
    key <- waitForKey
    if gFinished game then
        return $ Right game
        else
        let player = gPlayer game
            playField = gPlayField game
            uPlayer = updatePlayerPosition key player playField
            gen = gRandomGen game
            monsters = gMonsters game
            (uMonsters, uGen) = moveMonsters monsters playField gen
            pGame = game { gPlayer = uPlayer, gMonsters = uMonsters, gRandomGen = uGen}
            tGame = checkTreasureCollection pGame
            uGame = checkGameEndingConditions tGame
        in gameLoop uGame


printFinishedGame :: PacManGame -> IO ()
printFinishedGame game = do 
    let score = gScore game
    putStrLn ""
    putStrLn $ "Your score is: " ++ show score ++ "!"
            
waitForKey :: IO (Maybe Char)
waitForKey = do
      gameLoopDelay
      ready <- hReady stdin
      if ready
        then do
            key <- hGetChar stdin
            _ <- clearInputBuffer
            return $ Just key
        else return Nothing

clearInputBuffer :: IO ()
clearInputBuffer = do
    ready <- hReady stdin
    if ready
        then do
            _ <- getChar
            clearInputBuffer
        else return ()

checkGameEndingConditions :: PacManGame -> PacManGame
checkGameEndingConditions game =
    let monsters = gMonsters game
        monstersPositions = map mPosition monsters
        playerPosition = pPosition (gPlayer game)
        monstersOnPlayer = any (\x -> x == playerPosition) monstersPositions
    in if monstersOnPlayer 
        then game { gFinished = True }
        else game