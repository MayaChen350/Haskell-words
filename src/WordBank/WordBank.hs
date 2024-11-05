module WordBank.WordBank (createWordBank) where
import Data.List (nub, sort)
import Data.Char (toLower, isAscii, isLetter)

-- | Create the string containing the word bank.
createWordBank :: IO String
createWordBank = do
    fileText <- readFile file
    return $
        makeStringFromWordList $
        textToWordList fileText


-- | Where the text file to have the word bank created from is.
file :: FilePath
file = "data/text.txt"

-- | Transform a text to a list of words without duplicate. Only count words with only ascii characters.
textToWordList :: String -> [String]
textToWordList = sort .
        filter (not . null) .
        nub . -- remove duplicate elements
        map (filter isLetter . filter isAscii . map toLower) .
        words

-- | Make the word list to a string by writing each elements between commas.
makeStringFromWordList :: [String] -> String
makeStringFromWordList listWordBank = 
    wordListToString listWordBank "" where
        
        wordListToString :: [String] -> String -> String
        wordListToString (x:xs) str = 
            case x:xs of
                -- if there's only one word remaining in the list
                [x] ->
                    str ++ ", " ++ x -- Return the list with the last word
                _ -> -- if anything else
                    if str == ""
                    then wordListToString xs $ str ++ x
                    else wordListToString xs $ str ++ ", " ++ x