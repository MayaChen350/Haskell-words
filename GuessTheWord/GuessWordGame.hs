module GuessTheWord.GuessWordGame (guessWordGame) where
import Enum (Difficulty(..))
import Control.Monad.State

guessWordGame:: Difficulty -> IO ()
guessWordGame dif = do
    let gameSettings = setSettings dif
    case difficulty gameSettings of
        SigmaMale ->
            void $ putStrLn "You're keeping your edging streak I see...\nBut even in the middle of the rain a lone wolf must be wet.\nIf you want to keep mewing for the legendary level 5 G Y A T T you must keep the grind and succeed at this challenge.\nGoku is watching you. Do not fail this bro."
        _ ->
            void $ putStrLn $ "Try to guess the word!\nYou have " ++ show (lives gameSettings) ++ " lives."

data GameSettings = GameSettings {lives :: Int, hiddenLettersPercent :: Float, difficulty :: Difficulty}

setSettings :: Difficulty -> GameSettings
setSettings difficulty = case difficulty of
        Easy -> GameSettings { lives = 7, hiddenLettersPercent = 1 / 100, difficulty = Easy }
        Normal -> GameSettings { lives = 5, hiddenLettersPercent = 1 / 100, difficulty = Normal }
        Hard -> GameSettings { lives = 3, hiddenLettersPercent = 1 / 100, difficulty = Hard }
        Hardcore -> GameSettings { lives = 1, hiddenLettersPercent = 1 / 100, difficulty = Hardcore }
        SigmaMale -> GameSettings { lives = 1, hiddenLettersPercent = 90 / 100, difficulty = SigmaMale }