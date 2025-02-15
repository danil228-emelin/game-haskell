{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module GameLogic where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

scaleFactor :: Float
scaleFactor = 20

type Position = (Float, Float)

type Velocity = (Float, Float)

data GameMode = Playing | StartScreen | EndScreen | Quit
  deriving (Eq)

data GameState = GameState
  { paddlePos :: Float,
    ballPos :: Position,
    ballVel :: Velocity,
    brickPositions :: [Position],
    score :: Int,
    difficulty :: String,
    lives :: Int,
    mode :: GameMode,
    boostActive :: Bool,
    boostDuration :: Float,
    slowDownDuration :: Float
  }

initialGameState :: GameState
initialGameState =
  GameState
    0
    (-10, -20)
    (8, 16)
    ([(x, y) | x <- [-10.9, -8.9 .. 9.1], y <- [2, 4 .. 8]] ++ [(0, 5), (5, 6)])
    0
    "Normal"
    3
    StartScreen
    False
    0
    0

renderStartScreen :: Picture
renderStartScreen = scale 0.2 0.2 $ translate (-4500) 0 $ text "Press 'S' to Start."

renderEndScreen :: Int -> Picture
renderEndScreen score =
  let gameFinishedText = scale 0.2 0.2 $ translate (-200) 50 $ text "Game Finished!"
      scoreText = scale 0.2 0.2 $ translate (-200) (-100) $ text ("Your Score: " ++ show score)
      restartText = scale 0.2 0.2 $ translate (-200) (-300) $ text "Press 'R' to Restart or 'Q' to Quit."
   in gameFinishedText <> scoreText <> restartText

createCircle :: Bool -> Color -> Picture
createCircle condition colorValue
  | condition = color colorValue $ thickCircle 3 99
  | otherwise = blank

getSpeed :: String -> Float
getSpeed "Easy" = 0.5
getSpeed "Normal" = 1.0
getSpeed "Hard" = 1.5
getSpeed _ = 1.0

createSquare :: Float -> Position -> Color -> Picture
createSquare length corner colorValue = color colorValue $ polygon (createSquarePath length corner)

createSquarePath :: Float -> Position -> Path
createSquarePath length (x, y) = [(x, y), (x + length, y), (x + length, y + length), (x, y + length), (x, y)]

renderGame :: GameState -> Picture
renderGame gameState@GameState {..} =
  case mode of
    StartScreen -> renderStartScreen
    EndScreen -> renderEndScreen score
    Playing -> renderPlayingGame gameState

renderPlayingGame :: GameState -> Picture
renderPlayingGame GameState {..} =
  let loseOverlay
        | fst ballPos < -20 || lives < 1 = color red (rectangleSolid 100 100)
        | otherwise = mempty
      winOverlay
        | null brickPositions && lives > 0 = color green (rectangleSolid 100 100)
        | otherwise = mempty
      lastLine = line [(-11, -11), (11, -11)]
      paddleLine = line [(paddlePos - 2, -10), (paddlePos + 2, -10)]
      ball = translate (fst ballPos) (snd ballPos) (circle 1)
      bricks = foldMap (\pos -> createSquare 1.8 pos (brickColor pos)) brickPositions
      scoreText = translate 5 17 (scale 0.01 0.01 (text ("Score: " ++ show score)))
      difficultyText = translate 5 15 (scale 0.01 0.01 (text ("Difficulty: " ++ difficulty)))
      livesText = translate 5 13 (scale 0.01 0.01 (text ("Lives: " ++ show lives)))
   in scale scaleFactor scaleFactor $
        loseOverlay
          <> winOverlay
          <> lastLine
          <> paddleLine
          <> ball
          <> bricks
          <> scoreText
          <> difficultyText
          <> livesText

brickColor :: Position -> Color
brickColor pos
  | isGreenBrick pos = green
  | isRedBrick pos = red
  | otherwise = blue

isGreenBrick :: Position -> Bool
isGreenBrick (x, y) = (x, y) == (0, 5)

isRedBrick :: Position -> Bool
isRedBrick (x, y) = (x, y) == (5, 6)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 's') Down _ _) state = state {mode = Playing}
handleInput (EventKey (Char 'r') Down _ _) state = initialGameState
handleInput (EventKey (Char 'q') Down _ _) state = error "Game Quit"
handleInput (EventKey (Char 'e') Down _ _) state = state {difficulty = "Easy"}
handleInput (EventKey (Char 'n') Down _ _) state = state {difficulty = "Normal"}
handleInput (EventKey (Char 'h') Down _ _) state = state {difficulty = "Hard"}
handleInput (EventKey (Char 'b') Down _ _) state = state {boostActive = False}
handleInput (EventMotion (paddleX, _)) state
  | mode state == Playing = state {paddlePos = paddleX / scaleFactor}
  | otherwise = state -- Move paddle only if playing
handleInput _ currentState = currentState

updateGame :: Float -> GameState -> GameState
updateGame elapsedTime gameState@GameState {..} =
  case mode of
    Playing ->
      let updatedSlowDownDuration = max 0 (slowDownDuration - elapsedTime)

          updatedBoostDuration = if boostActive then boostDuration - elapsedTime else boostDuration
          isBoostActive = updatedBoostDuration > 0

          (newBallPos, newBallVel, newLives, newMode) =
            case () of
              _
                | snd ballPos < -20 && lives > 1 ->
                    ((0, -20), (8, 16), lives - 1, Playing)
              _
                | snd ballPos < -20 ->
                    ((0, -20), (0, 0), 0, EndScreen)
              _ ->
                let speedMultiplier = getSpeed difficulty * (if isBoostActive then 2 else 1.0) * (if updatedSlowDownDuration > 0 then 0.5 else 1.0)
                    newBallX = fst ballPos + fst ballVel * elapsedTime * speedMultiplier
                    newBallY = snd ballPos + snd ballVel * elapsedTime * speedMultiplier
                 in ((newBallX, newBallY), ballVel, lives, Playing)

          updatedBricks = filter (\(brickX, brickY) -> brickX > fst newBallPos || brickX + 2 < fst newBallPos || brickY > snd newBallPos || brickY + 2 < snd newBallPos) brickPositions

          newScore = if brickPositions /= updatedBricks then score + 1 else score
          newBoostActive = any (\(brickX, brickY) -> brickX <= fst newBallPos && fst newBallPos <= brickX + 2 && brickY <= snd newBallPos && snd newBallPos <= brickY + 2) (filter isGreenBrick brickPositions)

          finalBoostDuration = if newBoostActive then 3.0 else updatedBoostDuration
          newSlowDownDuration = if newBoostActive then 3.0 else updatedSlowDownDuration

          (newVelX, newVelY)
            | snd newBallPos < -10 && snd newBallPos > -11 && fst newBallPos > paddlePos - 2 && fst newBallPos < paddlePos + 2 =
                ((fst newBallPos - paddlePos) * 10, abs (snd newBallVel))
            | fst newBallPos < -10 || fst newBallPos > 10 =
                (-abs (fst newBallVel) * signum (fst newBallPos), snd newBallVel)
            | snd newBallPos > 10 || brickPositions /= updatedBricks =
                (fst newBallVel, -abs (snd newBallVel))
            | otherwise = (fst newBallVel, snd newBallVel)
       in gameState
            { ballPos = newBallPos,
              ballVel = (newVelX, newVelY),
              brickPositions = updatedBricks,
              score = newScore,
              lives = newLives,
              mode = newMode,
              boostActive = newBoostActive,
              boostDuration = finalBoostDuration,
              slowDownDuration = newSlowDownDuration
            }
    StartScreen -> gameState
    EndScreen -> gameState
