module WordBank.WordBank (createWordBank) where
import Data.List (nub, sort)
import Distribution.Compat.CharParsing (CharParsing(string))
import Data.Char (toLower, isAscii, isLetter)

createWordBank :: IO String
createWordBank =
    makeString .
    makeNoDuplicateList .
    readFile $ file

file :: FilePath
file = "data/text.txt"

makeNoDuplicateList :: IO String -> IO [String]
makeNoDuplicateList ioString = do
    string <- ioString
    return $
        sort .
        filter (not . null) .
        nub .
        map (filter isLetter . filter isAscii . map toLower) .
        words $ string

makeString :: IO [String] -> IO String
makeString l = do
    listWordBank <- l
    return $ listToString listWordBank ""

listToString :: [String] -> String -> String
listToString (x:xs) str = case x:xs of
    [x] ->
        str ++ ", " ++ x
    _ ->
        if str /= ""
        then listToString xs
            $ str ++ ", " ++ x
        else listToString xs
            $ str ++ x