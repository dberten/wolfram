module Main where
import Lib
import System.Environment
import System.Exit
import Text.Read
import Data.Maybe

data Conf = Info {
    start :: Maybe Int,
    line :: Maybe Int,
    rule :: Maybe Int,
    moves :: Maybe Int,
    window :: Maybe Int
} deriving (Show)

defaultConf :: Conf
defaultConf = Info {
    start = Just 0,
    line = Nothing,
    rule = Nothing,
    moves = Nothing,
    window = Just 80
}

getMin :: String -> Int -> Maybe Int
getMin str min = readMaybe str >>= \nb -> if nb >= min then Just nb else Nothing
 
readNb :: String -> Int -> Int -> Maybe Int
readNb str min max = readMaybe str >>= \nb -> case min <= nb && nb <= max of
     True -> Just nb
     False -> Nothing
    
getOption :: Conf -> [String] -> Maybe Conf
getOption info [] = case rule info of
    Nothing -> Nothing
    Just _ -> Just info
getOption _ [_] = Nothing
getOption info (x:y:xs) = case x of
    "--window" -> getOption info {window = getMin y 0} xs
    "--lines" -> getOption info {line = getMin y 0} xs
    "--start" -> getOption info {start = getMin y 0} xs
    "--move" -> getOption info {moves = readMaybe y} xs
    "--rule" -> getOption info {rule = readNb y 0 255} xs
    _ -> Nothing

decToBin :: Int -> [Int]
decToBin 0 = [0]
decToBin 1 = [1]
decToBin n | n `mod` 2 == 1 = decToBin (n `div` 2) ++ [1]
           | n `mod` 2 == 0 = decToBin (n `div` 2) ++ [0]

setRule :: [Int] -> [Int]
setRule x | length x < 8 = setRule (0 : x)
          | otherwise = x

numToChar :: [Int] -> [Char]
numToChar [] = []
numToChar (x:xs) | x == 1 = '*' : numToChar xs
                | x == 0 = ' ' : numToChar xs

getRule :: Char -> Char -> Char -> [Char] -> Char
getRule a b c d | a == '*' && b == '*' && c == '*' = head d
                | a == '*' && b == '*' && c == ' ' = d!!1
                | a == '*' && b == ' ' && c == '*' = d!!2
                | a == '*' && b == ' ' && c == ' ' = d!!3
                | a == ' ' && b == '*' && c == '*' = d!!4
                | a == ' ' && b == '*' && c == ' ' = d!!5
                | a == ' ' && b == ' ' && c == '*' = d!!6
                | a == ' ' && b == ' ' && c == ' ' = d!!7

generateLine :: [Char] -> [Char] -> [Char]
generateLine (a:b:c:as) rule = getRule a b c rule : generateLine (b:c:as) rule
generateLine _ _ = []

marginLines :: Int -> [Char] -> [Char]
marginLines x str | x <= 0 = ' ' : str
marginLines width str = ' ' : marginLines (width - 1) str ++ [' ']

printLines :: Int -> Int -> [Char] -> [Char] -> IO ()
printLines 0 _ _ _ = return ()
printLines lines width prev rule | length prev <= width = putStrLn (marginLines (width `div` 2 - (length prev `div` 2) - 1) prev) >> printLines (lines - 1) width (generateLine ("  " ++ prev ++ "  ") rule) rule
                                 | otherwise = putStrLn (take width (drop ((length prev `div` 2) - (width `div` 2)) prev)) >> printLines (lines - 1) width (generateLine ("  " ++ prev ++ "  ") rule) rule

getNb :: Maybe Int -> Int
getNb Nothing = -84
getNb (Just x) = x

main :: IO ()
main = getArgs >>= \args -> case getOption defaultConf args of
    Just info -> printLines (getNb (line info)) (getNb(window info)) "*" (numToChar(setRule(decToBin (getNb(rule info)))))
    Nothing -> putStrLn "Bad config" >> exitWith (ExitFailure 84)