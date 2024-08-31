import System.Environment ( getArgs )
import WordBank.WordBank (createWordBank)
import GuessTheWord.GuessWordGame (guessWordGame)
import Enum ( Difficulty )
import Distribution.Compat.Prelude (readMaybe)


main :: IO ()
main = do
    args <- getArgs
    wordBank <- createWordBank
    case head args of
        "MakeWordBank" ->
            writeFile "data/word_bank.txt" wordBank
        "Game" ->
            case readMaybe $ args !! 1 :: Maybe Difficulty of
                Just difficulty -> guessWordGame difficulty
                Nothing -> putStrLn "Incorrect. Please choose a difficulty."
        _ ->
            putStr "Error"