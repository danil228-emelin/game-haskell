module Audio where

import System.Process (system)
import System.Directory (doesFileExist)

-- Function to play a sound file
playSound :: String -> IO ()  -- Corrected the type signature
playSound soundFile = do
    _ <- system $ "aplay " ++ soundFile  -- Use the appropriate command for your OS
    return ()

-- Function to handle ball hitting a brick
ballHit :: IO ()  -- Added type signature
ballHit = do
    let soundFile = "sounds/lose_life.wav"  -- Ensure this path is correct
    fileExists <- doesFileExist soundFile
    if fileExists
        then playSound soundFile
        else putStrLn $ "Sound file not found: " ++ soundFile
