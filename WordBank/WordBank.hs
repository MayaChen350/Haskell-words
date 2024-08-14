module WordBank.WordBank (createWordBank) where

createWordBank :: IO String
createWordBank = readFile "./data/text.txt"