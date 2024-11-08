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
import Immutable.Shuffle ( shuffleM, shuffle )
import qualified Data.Vector as V ( Vector, fromList, toList, (++), head, tail, empty, filterM)
import Data.List.Split ( splitOn )
import GHC.Float ( int2Float )
import System.Random (randomRIO)
import Data.List.NonEmpty (append)
import Control.Monad (filterM)
import System.Directory (doesFileExist)

data GameSettings = GameSettings {lives :: Int, hiddenLettersPercent :: Float, difficulty :: Difficulty, score :: Int, unfoundWords :: V.Vector String}

type GameState = StateT GameSettings IO

-- | Start the game.
guessWordGame:: Difficulty -> IO ()
guessWordGame dif = do
    textFileExist <- doesFileExist "data/text.txt"
    if textFileExist
        then do
            wordBankFileExist <- doesFileExist "data/word_bank.txt"
            if wordBankFileExist
                then do
                    wordBank <- wordBankList
                    gameSettings <- execStateT (setSettings dif) (GameSettings 0 0 dif 0 wordBank)
                    evalStateT gameIntro gameSettings
                else liftIO $ print "You need to do the MakeWordBank command before playing the game!"
        else liftIO $ print "Wait. Create data/text.txt first. The program was going to crash otherwise. Don't forget to put text in it and do the MakeWordBank command!"

-- | Call the introduction message to the game.
gameIntro :: GameState ()
gameIntro = do
    gameState <- get
    filterWordBank
    if unfoundWords gameState == V.empty
        then liftIO $ print "Erm. The list of words is empty...? I think it's because you took too short words for that type of difficulty. Try going at a higher difficulty or changing the word bank!"
        else do
            liftIO . putStrLn $
                case difficulty gameState of
                    SigmaMale ->
                        "You're keeping your edging streak I see...\nBut even in the middle of the rain a lone wolf must be wet.\nIf you want to keep mewing for the legendary level 5 G Y A T T you must keep the grind and succeed at this challenge.\nGoku is watching you. Do not fail this bro."
                    _ ->
                        "Try to guess the words!\nYou have " ++ show (lives gameState) ++ " lives."
            gameLoop -- Start the gameLoop

gameLoop :: GameState ()
gameLoop = do
    gameState <- get
    let wordToFind = V.head $ unfoundWords gameState

    -- number of hidden characters based on game difficulty
    numHiddenChars <- getNumHiddenChars wordToFind

    -- String with the hidden indexes of the word
    hiddenOutputs <- liftIO $ hiddenOutput (length wordToFind) numHiddenChars

    let processedWord = processWord wordToFind hiddenOutputs
    liftIO $ print (fst processedWord)

    hangmanResult <- hangmanGambitLoop processedWord

    if hangmanResult then
        if V.empty == unfoundWords gameState
            then outOfWords
            else gameLoop
    else gameOver

hangmanGambitLoop :: (String, String) -> GameState Bool
hangmanGambitLoop wordTuple = do
    gameState <- get
    guess <- liftIO getLine

    let pointsForWord = 10 * length (filter (== '_') (fst wordTuple))

    if lives gameState == 0
        then return False
    else
        if guess == snd wordTuple then
            liftIO (putStrLn "You got it!") >> win pointsForWord >> return True
        else
            liftIO (putStrLn "Wrong word.") >> die >> hangmanGambitLoop wordTuple

-- | Make the word bank list from the word_bank.txt file. Shuffle the words.
wordBankList :: IO (V.Vector String)
wordBankList = do
    wordList <- splitOn ", " <$> readFile "data/word_bank.txt"
    if wordList == [[]]
        then return V.empty
        else shuffleM (V.fromList wordList)

-- | Remove all the words without hidden letters.
filterWordBank :: GameState ()
filterWordBank = do
    gameState <- get
    newWordList <- V.filterM filterWordBankPredicate (unfoundWords gameState)
    modify (\gs -> gs {unfoundWords = newWordList})

filterWordBankPredicate :: String ->  GameState Bool
filterWordBankPredicate str = do
    numHiddenChars <- getNumHiddenChars str
    return $ numHiddenChars /= 0

-- | Make a string with hidden characters out of the original word.
processWord :: String -> String -> (String, String)
processWord word hiddenOutput =
    (zipWith (\char hiddenChar -> if hiddenChar == '0' then '_' else char)
        word hiddenOutput, word) -- tuple: (hiddenWord, originalWord)

-- | Make a string out of hidden characters to define the hidden indexes of a word based on its length and a number of hidden characters.
hiddenOutput :: Int -> Int -> IO String
hiddenOutput wordLength numHiddenChars = do
    let outputs = replicate numHiddenChars 0 ++ replicate (wordLength - numHiddenChars) 1
    vector <- shuffleM (V.fromList outputs)
    return $ concatMap show $ V.toList vector

setSettings :: Difficulty -> GameState ()
setSettings difficulty =
        case difficulty of
            Easy -> modify (\gs -> gs { lives = 7, hiddenLettersPercent = 10 / 100 })
            Normal -> modify (\gs -> gs { lives = 5, hiddenLettersPercent = 20 / 100 })
            Hard -> modify (\gs -> gs { lives = 3, hiddenLettersPercent = 30 / 100 })
            Hardcore -> modify (\gs -> gs { lives = 1, hiddenLettersPercent = 30 / 100 })
            SigmaMale -> modify (\gs -> gs { lives = 1, hiddenLettersPercent = 70 / 100, score = 10 })

die :: GameState ()
die = do
    currentSettings <- get
    let l = lives currentSettings
    modify (\gs -> gs { lives = l - 1 })

win :: Int -> GameState ()
win pointsForWord = do
    gameState <- get
    let currentScore = score gameState
    modify (\gs -> gs {unfoundWords = V.tail $ unfoundWords gameState, score = currentScore + pointsForWord})

gameOver :: GameState ()
gameOver = do
    gameState <- get
    liftIO $ putStrLn ("Game Over. Your score was of " ++ show (score gameState))

outOfWords :: GameState ()
outOfWords = do
    gameState <- get
    liftIO $ putStrLn ("Congratulations! You guessed all the words this time. Your final score is of " ++ show (score gameState))

getNumHiddenChars :: String -> GameState Int
getNumHiddenChars wordToFind = do
    gameState <- get
    return $ floor (hiddenLettersPercent gameState * int2Float (length wordToFind))