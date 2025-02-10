#!/usr/bin/env -S stack script --compile --resolver lts-20.13 --package gloss

module Main where   
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

scaleFactor :: Float
scaleFactor = 20

type Position = (Float, Float)  -- Represents a position in 2D space
type Velocity = (Float, Float)  -- Represents velocity in 2D space

-- Define GameState as a data type
data GameState = GameState
    { paddlePos   :: Float       -- Paddle position
    , ballPos     :: Position     -- Ball position
    , ballVel     :: Velocity     -- Ball velocity
    , brickPositions :: [Position] -- Brick positions
    , score       :: Int          -- Score
    , difficulty   :: String      -- Difficulty level
    , lives       :: Int          -- Number of lives
    }

main :: IO ()
main = play FullScreen  -- Display mode 
            white -- Background color
            60   -- Frames per second
            (GameState 0 (-10, -20) (8, 16) ((,) <$> [-10.9, -8.9 .. 9.1]<*> [2, 4 .. 8]) 0 "Normal" 3)  -- Initial game state
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
renderGame (GameState paddlePos ballPos ballVel brickPositions score difficulty lives) = 
  let
    -- Create overlays as Pictures
    loseOverlay  = if fst ballPos < -20 || lives < 1 then color red (rectangleSolid 100 100) else mempty
    winOverlay   = if null brickPositions && lives > 0 then color green (rectangleSolid 100 100) else mempty

    -- Draw game elements
    lastLine     = line (createSquare 22 (-11, -11))
    paddleLine   = line [(paddlePos - 2, -10), (paddlePos + 2, -10)]
    ball         = translate (fst ballPos) (snd ballPos) (circle 1)
    bricks       = foldMap (polygon . createSquare 1.8) brickPositions

    -- Render the score, difficulty, and lives
    scoreText    = translate 5 17 (scale 0.01 0.01 (text ("Score: " ++ show score)))
    difficultyText = translate 5 15 (scale 0.01 0.01 (text ("Difficulty: " ++ difficulty)))
    livesText    = translate 5 13 (scale 0.01 0.01 (text ("Lives: " ++ show lives)))  -- Display lives

  in
    scale scaleFactor scaleFactor $ loseOverlay <> winOverlay <> lastLine <> paddleLine <> ball <> bricks <> scoreText <> difficultyText <> livesText

-- Input handler function
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'e') Down _ _) state = state { difficulty = "Easy" }
handleInput (EventKey (Char 'n') Down _ _) state = state { difficulty = "Normal" }
handleInput (EventKey (Char 'h') Down _ _) state = state { difficulty = "Hard" }
handleInput (EventMotion (paddleX, _)) state = state { paddlePos = paddleX / scaleFactor }
handleInput _ currentState = currentState

-- Update function
updateGame :: Float -> GameState -> GameState
updateGame elapsedTime gameState@(GameState paddlePos ballPos ballVel brickPositions score difficulty lives) =
    if snd ballPos < -20 then  -- Check if the ball has fallen below the paddle
        if lives > 1 then  -- Check if the player has lives left
            gameState { paddlePos = 0, ballPos = (0, -20), ballVel = (8, 16), lives = lives - 1 }  -- Reset ball and reduce lives
        else
            gameState { paddlePos = 0, ballPos = (0, -20), ballVel = (0, 0), score = 0, difficulty = "Normal", lives = 0 }  -- Reset game if no lives left
    else
        gameState { ballPos = (newBallX, newBallY), ballVel = (newVelX, newVelY), brickPositions = updatedBricks, score = newScore }
  where 
    -- Calculate new ball position based on difficulty speed
    speedMultiplier = getSpeed difficulty
    newBallX = fst ballPos + fst ballVel * elapsedTime * speedMultiplier
    newBallY = snd ballPos + snd ballVel * elapsedTime * speedMultiplier

    -- Filter out the bricks that have been hit by the ball
    updatedBricks = filter (\(brickX, brickY) -> brickX > newBallX || brickX + 2 < newBallX || brickY > newBallY || brickY + 2 < newBallY) brickPositions
    
    newScore = if brickPositions /= updatedBricks then score + 1 else score  -- Increase score when a brick is hit

    -- Bounce on paddle, adjust horizontal velocity
    (newVelX, newVelY) | newBallY < -10 && newBallY > -11 && newBallX > paddlePos - 2 && newBallX < paddlePos + 2 = ((newBallX - paddlePos) * 10, abs (snd ballVel))
                       -- Bounce on left and right walls
                       | newBallX < -10 || newBallX > 10 = (-abs (fst ballVel) * signum newBallX, snd ballVel)
                       | newBallY > 10 || brickPositions /= updatedBricks = (fst ballVel, -abs (snd ballVel))
                       | True = (fst ballVel, snd ballVel)

