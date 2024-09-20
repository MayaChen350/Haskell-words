module GuessTheWord.GuessWordGame (guessWordGame) where
import Enum (Difficulty(..))
import Control.Monad.State
    ( modify,
      evalStateT,
      execStateT,
      MonadIO(liftIO),
      MonadState(get, put),
      StateT )
import Control.Exception ( throw )
import Immutable.Shuffle ( shuffleM )
import qualified Data.Vector as V
import Data.List.Split ( splitOn )
import GHC.Float ( int2Float )
import System.Random (randomRIO)

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
    playGame

playGame :: GameState ()
playGame = do
    gameState <- get
    wordToFind <- liftIO findRandomWord
    processedWord <- liftIO $ processWord wordToFind (hiddenLettersPercent gameState)
    liftIO $ putStrLn processedWord

wordBankList :: IO [String]
wordBankList = splitOn ", " <$> readFile "data/word_bank.txt"

findRandomWord :: IO String
findRandomWord = do
    wordList <- wordBankList
    randomIndex <- randomRIO (0, length wordList)
    return $ wordList !! randomIndex
    
processWord :: String -> Float -> IO String
processWord word hiddenLettersPercent = do
    hiddenList <- hiddenOutput (length word) numHiddenChars
    return $ zipWith
        (\char hiddenChar -> if hiddenChar == '0' then '_' else char)
        word hiddenList
    where numHiddenChars = floor (hiddenLettersPercent * int2Float (length word))

hiddenOutput :: Int -> Int -> IO String
hiddenOutput wordLength numHiddenChars = do
    let outputs = replicate numHiddenChars 0 ++ replicate (wordLength - numHiddenChars) 1
    vector <- shuffleM (V.fromList outputs)
    return $ concatMap show $ V.toList vector


setSettings :: Difficulty -> GameState()
setSettings difficulty = case difficulty of
        Easy -> put GameSettings { lives = 7, hiddenLettersPercent = 10 / 100, difficulty = Easy }
        Normal -> put GameSettings { lives = 5, hiddenLettersPercent = 20 / 100, difficulty = Normal }
        Hard -> put GameSettings { lives = 3, hiddenLettersPercent = 30 / 100, difficulty = Hard }
        Hardcore -> put GameSettings { lives = 1, hiddenLettersPercent = 30 / 100, difficulty = Hardcore }
        SigmaMale -> put GameSettings { lives = 1, hiddenLettersPercent = 70 / 100, difficulty = SigmaMale }

die :: GameState ()
die = do
    currentSettings <- get
    let l = lives currentSettings
    modify (\gs -> gs { lives = l - 1 })