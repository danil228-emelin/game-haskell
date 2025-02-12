#!/usr/bin/env -S stack script --compile --resolver lts-20.13 --package gloss

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

-- Constants
scaleFactor :: Float
scaleFactor = 20

-- Type Definitions
type Position = (Float, Float)  -- Represents a position in 2D space
type Velocity = (Float, Float)  -- Represents velocity in 2D space

-- Game Modes
data GameMode = Playing | StartScreen | EndScreen | Quit deriving Eq

-- Game State Data Structure
data GameState = GameState {
    paddlePos :: Float,
    ballPos :: Position,
    ballVel :: Velocity,
    brickPositions :: [Position],
    score :: Int,
    difficulty :: String,
    lives :: Int,
    mode :: GameMode,
    boostActive :: Bool,
    boostDuration :: Float,
    slowDownDuration :: Float  -- New field to track slowdown duration
}

-- Initial Game State
initialGameState :: GameState
initialGameState = GameState 
    0                        -- Paddle position
    (-10, -20)              -- Ball position
    (8, 16)                 -- Ball velocity
    ([(x, y) | x <- [-10.9, -8.9 .. 9.1], y <- [2, 4 .. 8]] ++ [(0, 5), (5, 6)])  -- Brick positions
    0                        -- Initial score
    "Normal"                -- Default difficulty
    3                        -- Lives
    StartScreen              -- Starting mode
    False                    -- Boost active
    0                        -- Boost duration
    0                        -- Slow down duration

-- Render the start screen
renderStartScreen :: Picture
renderStartScreen = scale 0.2 0.2 $ translate (-4500) 0 $ text 
    "Welcome to the Brick Breaker Game!\nPress 'S' to Start.\nYou have 3 lives.\n- Press 'E' for Easy, 'N' for Normal, 'H' for Hard."

-- Render the end screen
renderEndScreen :: Int -> Picture
renderEndScreen score = 
    let
        gameFinishedText = scale 0.2 0.2 $ translate (-200) 50 $ text "Game Finished!"
        scoreText = scale 0.2 0.2 $ translate (-200) (-100) $ text ("Your Score: " ++ show score)
        restartText = scale 0.2 0.2 $ translate (-200) (-300) $ text "Press 'R' to Restart or 'Q' to Quit."
    in
        gameFinishedText <> scoreText <> restartText

-- Main Function
main :: IO ()
main = play FullScreen  -- Display mode 
            white       -- Background color
            60          -- Frames per second
            initialGameState
            renderGame   -- Render function
            handleInput  -- Input handler function
            updateGame   -- Update function

-- Create a colored circle if the condition is true
createCircle :: Bool -> Color -> Picture
createCircle condition colorValue 
    | condition = color colorValue $ thickCircle 3 99
    | otherwise = blank

-- Get speed multiplier based on difficulty
getSpeed :: String -> Float
getSpeed "Easy"   = 0.5
getSpeed "Normal" = 1.0
getSpeed "Hard"   = 1.5
getSpeed _        = 1.0  -- Default case

-- Create a colored square shape
createSquare :: Float -> Position -> Color -> Picture
createSquare length corner colorValue = color colorValue $ polygon (createSquarePath length corner)

-- Create the path for a square shape
createSquarePath :: Float -> Position -> Path
createSquarePath length (x, y) = [(x, y), (x + length, y), (x + length, y + length), (x, y + length), (x, y)]

-- Render function
renderGame :: GameState -> Picture
renderGame (GameState paddlePos ballPos ballVel brickPositions score difficulty lives mode boostActive boostDuration slowDownDuration) = 
    case mode of
        StartScreen -> renderStartScreen  -- Render the start screen
        EndScreen   -> renderEndScreen score  -- Render the end screen with the score
        Playing     -> renderPlayingGame paddlePos ballPos ballVel brickPositions score difficulty lives

-- Render Playing Game State
renderPlayingGame :: Float -> Position -> Velocity -> [Position] -> Int -> String -> Int -> Picture
renderPlayingGame paddlePos ballPos ballVel brickPositions score difficulty lives =
    let
        -- Create overlays as Pictures
        loseOverlay = if fst ballPos < -20 || lives < 1 
                      then color red (rectangleSolid 100 100) 
                      else mempty
        winOverlay = if null brickPositions && lives > 0 
                     then color green (rectangleSolid 100 100) 
                     else mempty

        -- Draw game elements
        lastLine = line [(-11, -11), (11, -11)]
        paddleLine = line [(paddlePos - 2, -10), (paddlePos + 2, -10)]
        ball = translate (fst ballPos) (snd ballPos) (circle 1)

        -- Render bricks with appropriate colors
        bricks = foldMap (\pos -> createSquare 1.8 pos (brickColor pos)) brickPositions

        -- Render score, difficulty, and lives
        scoreText = translate 5 17 (scale 0.01 0.01 (text ("Score: " ++ show score)))
        difficultyText = translate 5 15 (scale 0.01 0.01 (text ("Difficulty: " ++ difficulty)))
        livesText = translate 5 13 (scale 0.01 0.01 (text ("Lives: " ++ show lives)))  -- Display lives

    in
        scale scaleFactor scaleFactor $ loseOverlay <> winOverlay <> lastLine <> paddleLine <> ball <> bricks <> scoreText <> difficultyText <> livesText

-- Determine the color of a brick based on its position
brickColor :: Position -> Color
brickColor pos 
    | isGreenBrick pos = green
    | isRedBrick pos   = red
    | otherwise        = blue

-- Check if a brick is green
isGreenBrick :: Position -> Bool
isGreenBrick (x, y) = (x, y) == (0, 5)  -- Position of the green brick

-- Check if a brick is red
isRedBrick :: Position -> Bool
isRedBrick (x, y) = (x, y) == (5, 6)  -- Position of the red brick

-- Handle user input
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 's') Down _ _) state = state { mode = Playing }  -- Start the game
handleInput (EventKey (Char 'r') Down _ _) state = initialGameState  -- Restart the game
handleInput (EventKey (Char 'q') Down _ _) state = error "Game Quit"  -- Quit the game
handleInput (EventKey (Char 'e') Down _ _) state = state { difficulty = "Easy" }
handleInput (EventKey (Char 'n') Down _ _) state = state { difficulty = "Normal" }
handleInput (EventKey (Char 'h') Down _ _) state = state { difficulty = "Hard" }
handleInput (EventKey (Char 'b') Down _ _) state = state { boostActive = False, boostDuration = 0, slowDownDuration = 0 }  -- Turn off all boosts
handleInput (EventMotion (paddleX, _)) state = if mode state == Playing 
                                                 then state { paddlePos = paddleX / scaleFactor } 
                                                 else state  -- Move paddle only if playing
handleInput _ currentState = currentState

-- Update the game state
updateGame :: Float -> GameState -> GameState
updateGame elapsedTime gameState@(GameState paddlePos ballPos ballVel brickPositions score difficulty lives mode boostActive boostDuration slowDownDuration) =
    case mode of
        Playing -> 
            let
                -- Update the slowdown duration
                updatedSlowDownDuration = if slowDownDuration > 0 
                                           then slowDownDuration - elapsedTime 
                                           else 0

                -- Check for boost duration
                updatedBoostDuration = if boostActive 
                                        then boostDuration - elapsedTime 
                                        else boostDuration
                isBoostActive = updatedBoostDuration > 0

                -- Check if the ball has fallen below the paddle
                (newBallPos, newBallVel, newLives, newMode) = 
                    if snd ballPos < -20 
                    then if lives > 1 
                         then ((0, -20), (8, 16), lives - 1, Playing)  -- Deduct one life and reset the ball
                         else ((0, -20), (0, 0), 0, EndScreen)  -- No lives left, transition to EndScreen
                    else 
                        -- Ball is still in play, update its position and velocity
                        let
                            -- Calculate speed multiplier based on difficulty and active boosts
                            speedMultiplier = getSpeed difficulty * 
                                              (if isBoostActive then 0.5 else 1.0) * 
                                              (if updatedSlowDownDuration > 0 then 0.5 else 1.0)
                            newBallX = fst ballPos + fst ballVel * elapsedTime * speedMultiplier
                            newBallY = snd ballPos + snd ballVel * elapsedTime * speedMultiplier
                            newBallVel = ballVel  -- Keep the ball velocity unchanged for now
                            newLives = lives  -- Lives remain the same unless ball falls below paddle
                            newMode = Playing  -- Remain in playing mode

                        in ((newBallX, newBallY), newBallVel, newLives, newMode)

                -- Filter out the bricks that have been hit by the ball
                updatedBricks = filter (\(brickX, brickY) -> 
                    brickX > fst newBallPos || 
                    brickX + 2 < fst newBallPos || 
                    brickY > snd newBallPos || 
                    brickY + 2 < snd newBallPos) brickPositions
                
                newScore = if brickPositions /= updatedBricks 
                            then score + 1 
                            else score  -- Increase score when a brick is hit

                -- Check for collisions with green and red bricks
                newBoostActive = any (\(brickX, brickY) -> 
                    brickX <= fst newBallPos && 
                    fst newBallPos <= brickX + 2 && 
                    brickY <= snd newBallPos && 
                    snd newBallPos <= brickY + 2) 
                    (filter isGreenBrick brickPositions)

                newSpeedUpActive = any (\(brickX, brickY) -> 
                    brickX <= fst newBallPos && 
                    fst newBallPos <= brickX + 2 && 
                    brickY <= snd newBallPos && 
                    snd newBallPos <= brickY + 2) 
                    (filter isRedBrick brickPositions)

                -- Update boost duration if the boost is activated
                finalBoostDuration = if newBoostActive then 3.0 else updatedBoostDuration  -- Set boost duration to 3 seconds

                -- Set slow down duration if the green block is hit
                newSlowDownDuration = if newBoostActive then 3.0 else updatedSlowDownDuration

                -- Bounce on paddle, adjust horizontal velocity
                (newVelX, newVelY) = 
                    if snd newBallPos < -10 && snd newBallPos > -11 && 
                       fst newBallPos > paddlePos - 2 && fst newBallPos < paddlePos + 2 
                    then 
                        ((fst newBallPos - paddlePos) * 10, abs (snd ballVel))
                    else if fst newBallPos < -10 || fst newBallPos > 10 
                    then 
                        (-abs (fst ballVel) * signum (fst newBallPos), snd ballVel)  -- Bounce on left and right walls
                    else if snd newBallPos > 10 || brickPositions /= updatedBricks 
                    then 
                        (fst ballVel, -abs (snd ballVel))  -- Bounce off the top or hit a brick
                    else 
                        (fst ballVel, snd ballVel)  -- No collision, keep current velocity

            in
                gameState {
                    ballPos = newBallPos, 
                    ballVel = (newVelX, newVelY), 
                    brickPositions = updatedBricks, 
                    score = newScore, 
                    lives = newLives, 
                    mode = newMode, 
                    boostActive = newBoostActive, 
                    boostDuration = finalBoostDuration, 
                    slowDownDuration = newSlowDownDuration
                }

        StartScreen -> gameState  -- No updates needed in StartScreen mode
        EndScreen   -> gameState  -- No updates needed in EndScreen mode

