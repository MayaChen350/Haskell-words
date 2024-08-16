import System.Environment ( getArgs )
import WordBank.WordBank (createWordBank)

main :: IO ()
main = do
    args <- getArgs
    wordBank <- createWordBank
    case head args of
        "MakeWordBank" ->
            writeFile "data/word_bank.txt" wordBank
        _ ->
            putStr "Error"