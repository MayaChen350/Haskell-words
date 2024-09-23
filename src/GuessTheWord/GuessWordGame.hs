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

data GameSettings = GameSettings {lives :: Int, hiddenLettersPercent :: Float, difficulty :: Difficulty, score :: Int}

type GameState = StateT GameSettings IO

guessWordGame:: Difficulty -> IO ()
guessWordGame dif = do
    gameSettings <- execStateT (setSettings dif) (GameSettings 0 0 Easy 0)
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

    let numHiddenChars = floor (hiddenLettersPercent gameState * int2Float (length wordToFind))
    hiddenOutputs <- liftIO (hiddenOutput (length wordToFind) numHiddenChars)

    let processedWord = processWord wordToFind hiddenOutputs

    if gameState 
    hangmanGambit processedWord

hangmanGambit :: (String, String) -> IO ()
hangmanGambit wordTuple = do
    guess <- liftIO readLn

    let pointsForWord = 10 * length (filter (== '_') (snd wordTuple))

    if lives gameState == 0 
        then gameOver
    else
        if guess == snd wordTuple then
            increaseScore pointsForWord
        else 
            die >> hangmanGambit wordTuple

wordBankList :: IO [String]
wordBankList = splitOn ", " <$> readFile "data/word_bank.txt"

findRandomWord :: IO String
findRandomWord = do
    wordList <- wordBankList
    randomIndex <- randomRIO (0, length wordList)
    return $ wordList !! randomIndex

processWord :: String -> String -> (String, String)
processWord word hiddenOutput =
    (zipWith (\char hiddenChar -> if hiddenChar == '0' then '_' else char)
        word hiddenOutput, word) -- tuple: (hiddenWord, originalWord)

hiddenOutput :: Int -> Int -> IO String
hiddenOutput wordLength numHiddenChars = do
    let outputs = replicate numHiddenChars 0 ++ replicate (wordLength - numHiddenChars) 1
    vector <- shuffleM (V.fromList outputs)
    return $ concatMap show $ V.toList vector


setSettings :: Difficulty -> GameState()
setSettings difficulty = case difficulty of
        Easy -> put GameSettings { lives = 7, hiddenLettersPercent = 10 / 100, difficulty = Easy, score = 0}
        Normal -> put GameSettings { lives = 5, hiddenLettersPercent = 20 / 100, difficulty = Normal, score = 0 }
        Hard -> put GameSettings { lives = 3, hiddenLettersPercent = 30 / 100, difficulty = Hard, score = 0 }
        Hardcore -> put GameSettings { lives = 1, hiddenLettersPercent = 30 / 100, difficulty = Hardcore, score = 0 }
        SigmaMale -> put GameSettings { lives = 1, hiddenLettersPercent = 70 / 100, difficulty = SigmaMale, score = 10 }

die :: GameState ()
die = do
    currentSettings <- get
    let l = lives currentSettings
    modify (\gs -> gs { lives = l - 1 })

increaseScore :: Int -> GameState()
increaseScore = do
    gameState <- get
    let currentScore = score gameState
    modify (\gs -> gs {score = currentScore + pointsForWord})

gameOver :: GameState ()
gameOver = do
    gameState <- get
    liftIO $ print ("Game Over. Your score was of" ++ show (score gameState))