#!/usr/bin/env -S stack script --compile --resolver lts-20.13 --package gloss

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

-- Scaling factor
scaleFactor :: Float
scaleFactor = 20

type Position = (Float, Float)  -- Represents a position in 2D space
type Velocity = (Float, Float)  -- Represents velocity in 2D space
-- Game state: paddle position, ball position, ball velocity, brick positions, score
type GameState = (Float, Position, Velocity, [Position], Int)

main :: IO ()
main = play FullScreen  -- Display mode 
            white -- Background color
            60   -- Frames per second
            (0, (-10, -20), (8, 16), (,) <$> [-10.9, -8.9 .. 9.1] <*> [2, 4 .. 8], 0)  -- Initial game state
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

-- Render function
renderGame :: GameState -> Picture
renderGame (paddlePos, (ballX, ballY), _, brickPositions, score) = 
  let
    -- Create overlays as Pictures
    loseOverlay  = if ballY < -20 then color red (rectangleSolid 100 100) else mempty
    winOverlay   = if null brickPositions then color green (rectangleSolid 100 100) else mempty

    -- Draw game elements
    lastLine     = line (createSquare 22 (-11, -11))
    paddleLine   = line [(paddlePos - 2, -10), (paddlePos + 2, -10)]
    ball         = translate ballX ballY (circle 1)
    bricks       = foldMap (polygon . createSquare 1.8) brickPositions

    -- Render the score
    scoreText    = translate 5 15 (scale 0.01 0.01 (text ("Score: " ++ show score)))  -- Adjust position and scale
  in
    scale scaleFactor scaleFactor $ loseOverlay <> winOverlay <> lastLine <> paddleLine <> ball <> bricks <> scoreText

-- Input handler function
handleInput :: Event -> GameState -> GameState
handleInput (EventMotion (paddleX, _)) (_, ballPos, ballVel, brickPositions, score) = (paddleX / scaleFactor, ballPos, ballVel, brickPositions, score)
handleInput _ currentState = currentState

-- Update function
updateGame :: Float -> GameState -> GameState
-- t: elapsed time
-- paddlePos: Paddle position.
-- (ballX, ballY): Ball position.
-- (vX, vY): Ball velocity.
-- brickPositions: List of bricks
updateGame elapsedTime (paddlePos, (ballX, ballY), (vX, vY), brickPositions, score) = (paddlePos, (newBallX, newBallY), (newVelX, newVelY), updatedBricks, newScore)
  where 
    -- Calculate new ball position
    newBallX = ballX + vX * elapsedTime
    newBallY = ballY + vY * elapsedTime

    -- Filter out the bricks that have been hit by the ball
    updatedBricks = filter (\(brickX, brickY) -> brickX > ballX || brickX + 2 < ballX || brickY > ballY || brickY + 2 < ballY) brickPositions
    
    newScore = if brickPositions /= updatedBricks then score + 1 else score  -- Increase score when a brick is hit

    -- Bounce on paddle, adjust horizontal velocity
    (newVelX, newVelY) | ballY < -10 && ballY > -11 && ballX > paddlePos - 2 && ballX < paddlePos + 2 = ((ballX - paddlePos) * 10, abs vY)
                       -- Bounce on left and right walls
                       | ballX < -10 || ballX > 10 = (-abs vX * signum ballX,vY)
                       | ballY > 10 || brickPositions /= updatedBricks = (vX, -abs vY)
                       | True = (vX,vY)
