module Main where

import System.Console.ANSI
import System.Random

import GameLoop
import Game
import Rendering

waitForAnyKey :: IO ()
waitForAnyKey = do
  _ <- getChar
  return ()

checkTerminalSize :: IO (Either String (Int, Int))
checkTerminalSize = do
    let minHight = 30
        minLength = 30
    size <- getTerminalSize
    putStrLn $ "Terminal size is " ++  show size
    case size of
        Just (x, y)
            | x > minHight && y > minLength -> return $ Right (x,y)
            | otherwise ->  return $ Left "Error: terminal size is not big enough!"
        Nothing -> return $ Left "Error: getting terminal size!" 

runPacMan :: IO (Either String PacManGame)
runPacMan = do
    size <- checkTerminalSize
    randomValue <- randomIO :: IO Int
    case size of 
        Left error -> return $ Left error
        Right size -> do 
            prepareConsole
            let initialGame = initGame (30, 30) randomValue
            finishedGame <- gameLoop initialGame
            waitForAnyKey
            recoverConsole
            return finishedGame

main :: IO ()
main = do
    finishedGame <- runPacMan
    case finishedGame of
        Left error -> putStrLn $ "GAME ERROR: " ++ error
        Right game -> printFinishedGame game
