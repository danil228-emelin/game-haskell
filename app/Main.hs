#!/usr/bin/env -S stack script --compile --resolver lts-20.13 --package gloss

module Main where   
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

scaleFactor :: Float
scaleFactor = 20

type Position = (Float, Float)  -- Represents a position in 2D space
type Velocity = (Float, Float)  -- Represents velocity in 2D space
-- Game state: paddle position, ball position, ball velocity, brick positions, score, difficulty, lives
type GameState = (Float, Position, Velocity, [Position], Int, String, Int)

main :: IO ()
main = play FullScreen  -- Display mode 
            white -- Background color
            60   -- Frames per second
            (0, (-10, -20), (8, 16), (,) <$> [-10.9, -8.9 .. 9.1] <*> [2, 4 .. 8], 0, "Normal", 3)  -- Initial game state
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
renderGame (paddlePos, (ballX, ballY), (xV,yV), brickPositions, score, difficulty, lives) = 
  let
    -- Create overlays as Pictures
    loseOverlay  = if ballY < -20 || lives <1 then color red (rectangleSolid 100 100) else mempty
    winOverlay   = if null brickPositions && lives>0  then color green (rectangleSolid 100 100) else mempty

    -- Draw game elements
    lastLine     = line (createSquare 22 (-11, -11))
    paddleLine   = line [(paddlePos - 2, -10), (paddlePos + 2, -10)]
    ball         = translate ballX ballY (circle 1)
    bricks       = foldMap (polygon . createSquare 1.8) brickPositions

    -- Render the score, difficulty, and lives
    scoreText    = translate 5 17 (scale 0.01 0.01 (text ("Score: " ++ show score)))
    difficultyText = translate 5 15 (scale 0.01 0.01 (text ("Difficulty: " ++ difficulty)))
    livesText    = translate 5 13 (scale 0.01 0.01 (text ("Lives: " ++ show lives)))  -- Display lives

  in
    scale scaleFactor scaleFactor $ loseOverlay <> winOverlay <> lastLine <> paddleLine <> ball <> bricks <> scoreText <> difficultyText <> livesText

-- Input handler function
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'e') Down _ _) (_, ballPos, ballVel, brickPositions, score, _, lives) = (0, ballPos, ballVel, brickPositions, score, "Easy", lives)
handleInput (EventKey (Char 'n') Down _ _) (_, ballPos, ballVel, brickPositions, score, _, lives) = (0, ballPos, ballVel, brickPositions, score, "Normal", lives)
handleInput (EventKey (Char 'h') Down _ _) (_, ballPos, ballVel, brickPositions, score, _, lives) = (0, ballPos, ballVel, brickPositions, score, "Hard", lives)
handleInput (EventMotion (paddleX, _)) (_, ballPos, ballVel, brickPositions, score, difficulty, lives) = (paddleX / scaleFactor, ballPos, ballVel, brickPositions, score, difficulty, lives)
handleInput _ currentState = currentState

-- Update function
updateGame :: Float -> GameState -> GameState
updateGame elapsedTime (paddlePos, (ballX, ballY), (vX, vY), brickPositions, score, difficulty, lives) =
    if ballY < -20 then  -- Check if the ball has fallen below the paddle
        if lives > 1 then  -- Check if the player has lives left
            (0, (0, -20), (8, 16), brickPositions, score, difficulty, lives - 1)  -- Reset ball and reduce lives
        else
            (0, (0, -20), (0, 0), brickPositions, 0, "Normal", 0)  -- Reset game if no lives left
    else
        (paddlePos, (newBallX, newBallY), (newVelX, newVelY), updatedBricks, newScore, difficulty, lives)
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

