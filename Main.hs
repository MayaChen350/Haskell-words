import System.Environment ( getArgs )
import WordBank.WordBank (createWordBank)
import GuessTheWord.GuessWordGame (guessWordGame)
import Enum ( Difficulty )
import Distribution.Compat.Prelude (readMaybe, toLower)

difficulties :: String
difficulties = "[Easy]\n[Normal]\n[Hard]\n[Hardcore]\n[SigmaMale]"

main :: IO ()
main = do
    args <- getArgs
    case map toLower $ head args of
        "makewordbank" -> do
            wordBank <- createWordBank
            writeFile "data/word_bank.txt" wordBank
        "game" ->
            if length args == 1 then
                putStrLn "Please specify a difficulty:" >> putStrLn difficulties
            else
                case readMaybe (args !! 1) :: Maybe Difficulty of
                Just difficulty -> guessWordGame difficulty
                Nothing -> putStrLn "Incorrect. Please choose a difficulty:" >> putStrLn difficulties
        _ ->
            putStr "Error"