module Rendering where

import System.Console.ANSI
import Data.Array
import Control.Monad
import System.IO

import Game

prepareConsole :: IO ()
prepareConsole = do
  mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout]
  hSetEcho stdout False
  hideCursor
  clearScreenBlack

recoverConsole :: IO ()
recoverConsole = do
  clearScreen
  hSetEcho stdout True
  showCursor

clearScreenBlack :: IO ()
clearScreenBlack = do
  setSGR [SetColor Background Dull Black]
  clearScreen
  setSGR [Reset]

renderSlot :: Char -> Color -> Color-> (Int, Int) -> IO ()
renderSlot char color bgColor (x, y) = do
  setCursorPosition x y
  setSGR [SetColor Background Dull bgColor]
  setSGR [SetColor Foreground Vivid color]
  putChar char
  setSGR [Reset]

renderMaze :: PlayField -> IO ()
renderMaze playField = do
  let ((minRow, minCol), (maxRow, maxCol)) = bounds playField
  forM_ [minRow..maxRow] $ \x -> do
    forM_ [minCol..maxCol] $ \y -> do
        let position = playField ! (x, y)
        if position == MazeWall 
            then renderSlot 'W' White White (x, y)
            else pure ()

renderPlayer :: Player -> IO ()
renderPlayer player = do
    let char = pCharacter player
        position = pPosition player
    renderSlot char Yellow Black position

renderTreasure :: Treasure -> IO ()
renderTreasure treasure = do
    let char = tCharacter treasure
        position = tPosition treasure
    renderSlot char Green Black position    

renderTreasures :: [Treasure] -> IO ()
renderTreasures treasure = 
    mapM_ renderTreasure treasure

renderMonsters :: [Monster] -> IO ()
renderMonsters monsters = 
    mapM_ renderMonster monsters

renderMonster :: Monster -> IO ()
renderMonster monster = do
    let char = mCharacter monster
        position = mPosition monster
    renderSlot char Red Black position

renderGame :: PacManGame -> IO ()
renderGame game = do
    let maze = gPlayField game
        player = gPlayer game
        monsters = gMonsters game
        treasures = gTreasures game
    clearScreenBlack
    renderMaze maze
    renderPlayer player
    renderTreasures treasures
    renderMonsters monsters