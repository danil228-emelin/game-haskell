#!/usr/bin/env -S stack script --compile --resolver lts-20.13 --package gloss

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import GameLogic

main :: IO ()
main = play FullScreen  -- Display mode 
            white       -- Background color
            60          -- Frames per second
            initialGameState
            renderGame   -- Render function
            handleInput  -- Input handler function
            updateGame   -- Update function