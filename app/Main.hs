#!/usr/bin/env -S stack script --compile --resolver lts-20.13 --package gloss

module Main where   
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

scaleFactor :: Float
scaleFactor = 20

type Position = (Float, Float)  -- Represents a position in 2D space
type Velocity = (Float, Float)  -- Represents velocity in 2D space

data GameMode = Playing | StartScreen | EndScreen | Quit deriving Eq


data GameState = GameState
    { paddlePos   :: Float
    , ballPos     :: Position
    , ballVel     :: Velocity
    , brickPositions :: [Position]
    , score       :: Int
    , difficulty   :: String
    , lives       :: Int
    , mode        :: GameMode  -- New field for game mode
    }

initialGameState:: GameState
initialGameState = (GameState 0 (-10, -20) (8, 16) ((,) <$> [-10.9, -8.9 .. 9.1]<*> [2, 4 .. 8]) 0 "Normal" 3 StartScreen)
-- Render the start screen
renderStartScreen :: Picture
renderStartScreen = scale 0.2 0.2 $ translate (-4500) 0 $ text "Welcome to the Brick Breaker Game!\nPress 'S' to Start.\nYou have 3 lives.\n- Press 'E' for Easy, 'N' for Normal, 'H' for Hard."

-- Render the end screen
renderEndScreen :: Int -> Picture
renderEndScreen score = 
    let
        gameFinishedText = scale 0.2 0.2 $ translate (-200) 50 $ text "Game Finished!"
        scoreText = scale 0.2 0.2 $ translate (-200) (-100) $ text ("Your Score: " ++ show score)
        restartText = scale 0.2 0.2 $ translate (-200) (-300) $ text "Press 'R' to Restart or 'Q' to Quit."
    in
        gameFinishedText <> scoreText <> restartText

main :: IO ()
main = play FullScreen  -- Display mode 
            white -- Background color
            60   -- Frames per second
            initialGameState
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
renderGame (GameState paddlePos ballPos ballVel brickPositions score difficulty lives mode) = 
    case mode of
        StartScreen -> renderStartScreen  -- Render the start screen
        EndScreen   -> renderEndScreen score  -- Render the end screen with the score
        Playing     -> 
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

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 's') Down _ _) state = state { mode = Playing }  -- Start the game
handleInput (EventKey (Char 'r') Down _ _) state = initialGameState
handleInput (EventKey (Char 'q') Down _ _) state = error "Game Quit"
handleInput (EventKey (Char 'e') Down _ _) state = state { difficulty = "Easy" }
handleInput (EventKey (Char 'n') Down _ _) state = state { difficulty = "Normal" }
handleInput (EventKey (Char 'h') Down _ _) state = state { difficulty = "Hard" }
handleInput (EventMotion (paddleX, _)) state = if mode state == Playing then state { paddlePos = paddleX / scaleFactor } else state  -- Only move paddle if playing
handleInput _ currentState = currentState


updateGame :: Float -> GameState -> GameState
updateGame elapsedTime gameState@(GameState paddlePos ballPos ballVel brickPositions score difficulty lives mode) =
    case mode of
        Playing -> 
            if snd ballPos < -20 then  -- Check if the ball has fallen below the paddle
                if lives > 1 then  -- Check if the player has lives left
                    gameState { paddlePos = 0, ballPos = (0, -20), ballVel = (8, 16), lives = lives - 1 }  -- Reset ball and reduce lives
                else
                    gameState { paddlePos = 0, ballPos = (0, -20), ballVel = (0, 0), score = 0, difficulty = "Normal", lives = 0, mode = EndScreen }  -- Reset game if no lives left
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

        StartScreen -> gameState  -- No updates needed in StartScreen mode
        EndScreen   -> gameState  -- No updates needed in EndScreen mode
