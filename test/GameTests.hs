{-# LANGUAGE RecordWildCards #-}

module Main where

import Game
import Test.Hspec
-- Example GameState for testing
initialState :: GameState
initialState = (0, (0, 0), (1, 1), [(1, 1), (2, 2)], 0, "Normal")

main :: IO ()
main = hspec $ do
    describe "Game Logic Tests" $ do
        it "should update ball position correctly" $ do
            let elapsedTime = 1.0
            let (paddlePos, (ballX, ballY), (vX, vY), brickPositions, score, difficulty) = initialState
            let newState = updateGame elapsedTime initialState
            let (newPaddlePos, (newBallX, newBallY), (newVelX, newVelY), _, _, _) = newState
            
            -- Check if the ball position is updated correctly
            newBallX `shouldBe` (ballX + vX * elapsedTime * getSpeed difficulty)
            newBallY `shouldBe` (ballY + vY * elapsedTime * getSpeed difficulty)

        it "should increase score when a brick is hit" $ do
            let (paddlePos, (ballX, ballY), (vX, vY), brickPositions, score, difficulty) = initialState
            let newState = updateGame 1.0 initialState
            let (_, _, _, updatedBricks, newScore, _) = newState
            
            -- Check if the score has increased
            if brickPositions /= updatedBricks
                then newScore `shouldBe` (score + 1)
                else newScore `shouldBe` score

        it "should return correct speed multiplier based on difficulty" $ do
            getSpeed "Easy" `shouldBe` 0.5
            getSpeed "Normal" `shouldBe` 1.0
            getSpeed "Hard" `shouldBe` 1.5
