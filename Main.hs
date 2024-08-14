import System.Environment ( getArgs )
import WordBank.WordBank (createWordBank)

main :: IO ()
main = do
    args <- getArgs
    content <- createWordBank
    putStr content