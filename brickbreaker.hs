#!/usr/bin/env -S stack script --compile --resolver lts-20.13 --package gloss

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

scaleFactor :: Float
scaleFactor = 20

type Position = (Float, Float)  -- Represents a position in 2D space
type Velocity = (Float, Float)  -- Represents velocity in 2D space
-- Game state: paddle position, ball position, ball velocity, brick positions, score, difficulty
type GameState = (Float, Position, Velocity, [Position], Int, String)

main :: IO ()
main = play FullScreen  -- Display mode 
            white -- Background color
            60   -- Frames per second
            (0, (-10, -20), (8, 16), (,) <$> [-10.9, -8.9 .. 9.1] <*> [2, 4 .. 8], 0, "Normal")  -- Initial game state
            renderGame -- Render function
            handleInput -- Input handler function
            updateGame -- Update function

-- Function to create a colored circle if the condition is true
createCircle :: Bool -> Color -> Picture
createCircle condition colorValue | condition = color colorValue $ thickCircle 3 99
                                   | otherwise = blank

-- Function to create a square shape
createSquare :: Float -> (Float, Float) -> Path
createSquare length corner@(x, y) = [corner, (x + length, y), (x + length, y + length), (x, y + length), corner]

-- Function to get the speed multiplier based on difficulty
getSpeed :: String -> Float
getSpeed "Easy"   = 0.5
getSpeed "Normal" = 1.0
getSpeed "Hard"   = 1.5
getSpeed _        = 1.0  -- Default case

-- Render function
renderGame :: GameState -> Picture
renderGame (paddlePos, (ballX, ballY), _, brickPositions, score, difficulty) = 
  let
    -- Create overlays as Pictures
    loseOverlay  = if ballY < -20 then color red (rectangleSolid 100 100) else mempty
    winOverlay   = if null brickPositions then color green (rectangleSolid 100 100) else mempty

    -- Draw game elements
    lastLine     = line (createSquare 22 (-11, -11))
    paddleLine   = line [(paddlePos - 2, -10), (paddlePos + 2, -10)]
    ball         = translate ballX ballY (circle 1)
    bricks       = foldMap (polygon . createSquare 1.8) brickPositions

    -- Render the score and difficulty
    scoreText    = translate 5 17 (scale 0.01 0.01 (text ("Score: " ++ show score)))
    difficultyText    = translate 5 15 (scale 0.01 0.01 (text ("Difficulty: " ++ difficulty)))

  in
    scale scaleFactor scaleFactor $ loseOverlay <> winOverlay <> lastLine <> paddleLine <> ball <> bricks <> scoreText <> difficultyText

-- Input handler function
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'e') Down _ _) (_, ballPos, ballVel, brickPositions, score, _) = (0, ballPos, ballVel, brickPositions, score, "Easy")
handleInput (EventKey (Char 'n') Down _ _) (_, ballPos, ballVel, brickPositions, score, _) = (0, ballPos, ballVel, brickPositions, score, "Normal")
handleInput (EventKey (Char 'h') Down _ _) (_, ballPos, ballVel, brickPositions, score, _) = (0, ballPos, ballVel, brickPositions, score, "Hard")
handleInput (EventMotion (paddleX, _)) (_, ballPos, ballVel, brickPositions, score, difficulty) = (paddleX / scaleFactor, ballPos, ballVel, brickPositions, score, difficulty)
handleInput _ currentState = currentState

-- Update function
updateGame :: Float -> GameState -> GameState
-- t: elapsed time
-- paddlePos: Paddle position.
-- (ballX, ballY): Ball position.
-- (vX, vY): Ball velocity.
-- brickPositions: List of bricks
updateGame elapsedTime (paddlePos, (ballX, ballY), (vX, vY), brickPositions, score, difficulty) = 
    (paddlePos, (newBallX, newBallY), (newVelX, newVelY), updatedBricks, newScore, difficulty)
  where 
    -- Calculate new ball position based on difficulty speed
    speedMultiplier = getSpeed difficulty
    newBallX = ballX + vX * elapsedTime * speedMultiplier
    newBallY = ballY + vY * elapsedTime * speedMultiplier

    -- Filter out the bricks that have been hit by the ball
    updatedBricks = filter (\(brickX, brickY) -> brickX > ballX || brickX + 2 < ballX || brickY > ballY || brickY + 2 < ballY) brickPositions
    
    newScore = if brickPositions /= updatedBricks then score + 1 else score  -- Increase score when a brick is hit

    -- Bounce on paddle, adjust horizontal velocity
    (newVelX, newVelY) | ballY < -10 && ballY > -11 && ballX > paddlePos - 2 && ballX < paddlePos + 2 = ((ballX - paddlePos) * 10, abs vY)
                       -- Bounce on left and right walls
                       | ballX < -10 || ballX > 10 = (-abs vX * signum ballX, vY)
                       | ballY > 10 || brickPositions /= updatedBricks = (vX, -abs vY)
                       | True = (vX, vY)