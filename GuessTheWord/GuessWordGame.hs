module GuessTheWord.GuessWordGame (guessWordGame) where
import Enum (Difficulty(..))
import Control.Monad.State
import Control.Exception (throw, throwIO)
import System.Win32.DebugApi (DebugEventInfo(Exception))

data GameSettings = GameSettings {lives :: Int, hiddenLettersPercent :: Float, difficulty :: Difficulty}

type GameState = StateT GameSettings IO

guessWordGame:: Difficulty -> IO ()
guessWordGame dif = do
    gameSettings <- execStateT (setSettings dif) (GameSettings 0 0 Easy)
    evalStateT gameIntro gameSettings

gameIntro :: GameState ()
gameIntro = do
    gameSettings <- get
    case difficulty gameSettings of
            SigmaMale ->
                liftIO $ putStrLn "You're keeping your edging streak I see...\nBut even in the middle of the rain a lone wolf must be wet.\nIf you want to keep mewing for the legendary level 5 G Y A T T you must keep the grind and succeed at this challenge.\nGoku is watching you. Do not fail this bro."
            _ ->
                liftIO $ putStrLn $ "Try to guess the word!\nYou have " ++ show (lives gameSettings) ++ " lives."

playGame :: GameState ()
playGame = do
    throw (userError "Not Implemented yet")

setSettings :: Difficulty -> GameState()
setSettings difficulty = case difficulty of
        Easy -> put GameSettings { lives = 7, hiddenLettersPercent = 1 / 100, difficulty = Easy }
        Normal -> put GameSettings { lives = 5, hiddenLettersPercent = 1 / 100, difficulty = Normal }
        Hard -> put GameSettings { lives = 3, hiddenLettersPercent = 1 / 100, difficulty = Hard }
        Hardcore -> put GameSettings { lives = 1, hiddenLettersPercent = 1 / 100, difficulty = Hardcore }
        SigmaMale -> put GameSettings { lives = 1, hiddenLettersPercent = 90 / 100, difficulty = SigmaMale }

die :: GameState ()
die = do
    currentSettings <- get
    put GameSettings { lives = lives currentSettings - 1, hiddenLettersPercent = hiddenLettersPercent currentSettings, difficulty = difficulty currentSettings}