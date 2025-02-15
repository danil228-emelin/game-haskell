{-# LANGUAGE RecordWildCards #-}

module Main where

import GameLogic
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

data GameConfig = GameConfig
  { displayMode :: Display,
    backgroundColor :: Color,
    frameRate :: Int,
    initialState :: GameState,
    renderGameState :: GameState -> Picture,
    handleInputGame :: Event -> GameState -> GameState,
    updateGameState :: Float -> GameState -> GameState
  }

defaultGameConfig :: GameConfig
defaultGameConfig =
  GameConfig
    FullScreen
    white
    60
    initialGameState
    renderGame
    handleInput
    updateGame

main :: IO ()
main = do
  playGame defaultGameConfig

playGame :: GameConfig -> IO ()
playGame GameConfig {..} =
  play
    displayMode
    backgroundColor
    frameRate
    initialState
    renderGameState
    handleInputGame
    updateGameState
