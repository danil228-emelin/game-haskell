module Main where

import Test.HUnit
import Test.QuickCheck
import GameLogic  -- Import the game logic module
import Graphics.Gloss.Data.Color  -- Import colors (green, red, blue)

-- Unit tests for getSpeed
testGetSpeed :: Test
testGetSpeed = TestList [
    "Easy difficulty should return 0.5" ~: getSpeed "Easy" ~?= 0.5,
    "Normal difficulty should return 1.0" ~: getSpeed "Normal" ~?= 1.0,
    "Hard difficulty should return 1.5" ~: getSpeed "Hard" ~?= 1.5,
    "Unknown difficulty should return 1.0" ~: getSpeed "Unknown" ~?= 1.0
  ]

-- Property test for getSpeed
prop_getSpeedPositive :: String -> Property
prop_getSpeedPositive difficulty = 
    getSpeed difficulty > 0 ==> getSpeed difficulty > 0

-- Unit tests for brickColor
testBrickColor :: Test
testBrickColor = TestList [
    "Green brick at (0, 5) should return green" ~: brickColor (0, 5) ~?= green,
    "Red brick at (5, 6) should return red" ~: brickColor (5, 6) ~?= red,
    "Blue brick at (10, 10) should return blue" ~: brickColor (10, 10) ~?= blue
  ]

-- Unit tests for isGreenBrick
testIsGreenBrick :: Test
testIsGreenBrick = TestList [
    "Position (0, 5) should be a green brick" ~: isGreenBrick (0, 5) ~?= True,
    "Position (5, 6) should not be a green brick" ~: isGreenBrick (5, 6) ~?= False
  ]

-- Unit tests for isRedBrick
testIsRedBrick :: Test
testIsRedBrick = TestList [
    "Position (5, 6) should be a red brick" ~: isRedBrick (5, 6) ~?= True,
    "Position (0, 5) should not be a red brick" ~: isRedBrick (0, 5) ~?= False
  ]

-- Unit tests for initialGameState
testInitialGameState :: Test
testInitialGameState = TestList [
    "Initial paddle position should be 0" ~: paddlePos initialGameState ~?= 0,
    "Initial ball position should be (-10, -20)" ~: ballPos initialGameState ~?= (-10, -20),
    "Initial ball velocity should be (8, 16)" ~: ballVel initialGameState ~?= (8, 16),
    "Initial score should be 0" ~: score initialGameState ~?= 0,
    "Initial difficulty should be Normal" ~: difficulty initialGameState ~?= "Normal",
    "Initial lives should be 3" ~: lives initialGameState ~?= 3
        ]

-- Property test for brickColor
prop_brickColorValid :: Position -> Property
prop_brickColorValid pos =
    property (brickColor pos `elem` [green, red, blue])  -- Ensure brickColor returns a valid color

-- Property test for isGreenBrick
prop_isGreenBrickValid :: Position -> Property
prop_isGreenBrickValid pos =
    isGreenBrick pos ==> pos == (0, 5)  -- Ensure isGreenBrick is only true for (0, 5)

-- Property test for isRedBrick
prop_isRedBrickValid :: Position -> Property
prop_isRedBrickValid pos =
    isRedBrick pos ==> pos == (5, 6)  -- Ensure isRedBrick is only true for (5, 6)

main :: IO ()
main = do
    runTestTT $ TestList [
        testGetSpeed,
        testBrickColor,
        testIsGreenBrick,
        testIsRedBrick,
        testInitialGameState
      ]
    quickCheck prop_getSpeedPositive
    quickCheck prop_brickColorValid
    quickCheck prop_isGreenBrickValid
    quickCheck prop_isRedBrickValid