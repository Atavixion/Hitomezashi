-- hitomezashi patters can be interpreted as binary, so the pattern '- - - ' -> '101010'
-- it can also be interpreted using opposite rules, like odd and even numbers, and vowels and constanasnts
-- they can be represented as rulesets

-- a string list and a ruleset should be given in, and a pattern to come out

-- eg ['10101', '01010'] [1 -> '= or ||', 0 -> '.']
-- ie [top, left] [ruleset]

module Main where

import qualified Graphics.Image as I
import Data.List
import Data.Char
import System.Environment
import System.IO
import Control.Applicative

type Pattern = I.Image I.VU I.YA Double

type Size = Int
type Len = Int
type Bit = Int

data Orientation = Horizontal | Vertical deriving (Show, Eq)

concatLR, concatTB :: [Pattern] -> Pattern
concatLR = foldl1 I.leftToRight
concatTB = foldl1 I.topToBottom

-- S CANNOT BE SMALLER THAN 3

genLn :: Orientation -> Size -> Len -> Bit -> Pattern
genLn Horizontal s len i = I.fromListsR I.VU $ if i == 1
                        then [take len $ cycle $ replicate s blk ++ replicate sp trns]
                    else if i == 0
                        then [take len $ trns: if last zlist == trns then init zlist else zlist] -- 0 lines have an exra space at the begin and end
                    else     [take len $ cycle $ replicate sp trns ++ replicate sp trns]
        where
            blk = I.PixelYA 0 1     -- black
            trns = I.PixelYA 0 0    -- transparent
            sp = s `mod` 3 + 1     -- spaces
            zlist = take len $ cycle $ replicate sp trns ++ replicate s blk
genLn Vertical s len i = I.transpose $ I.fromListsR I.VU $ if i == 1
                        then [take len $ cycle $ replicate s blk ++ replicate sp trns]
                    else if i == 0
                        then [take len $ trns: if last zlist == trns then init zlist else zlist] -- 0 lines have an exra space at the begin and end
                    else     [take len $ cycle $ replicate sp trns ++ replicate sp trns]
        where
            blk = I.PixelYA 0 1     -- black
            trns = I.PixelYA 0 0    -- transparent
            sp = s `mod` 3 + 1     -- spaces
            zlist = take len $ cycle $ replicate sp trns ++ replicate s blk

genTop, genLft :: Size -> Len -> [Bit] -> Pattern
genTop s len xs = concatLR $ map (genLn Vertical s len) (intercalate (replicate sp 2) (map (:[]) xs))
    where sp = s `mod` 3 + 1

genLft s len xs = concatTB $ map (genLn Horizontal s len) (intercalate (replicate sp 2) (map (:[]) xs))
    where sp = s `mod` 3 + 1

writeLft, writeTop :: Pattern -> IO ()
writeLft = I.writeImage "images/lft.png"
writeTop = I.writeImage "images/top.png"

writeHitomezashi :: String -> Size -> [[Bit]]-> IO ()
writeHitomezashi fileName s xs = I.writeImage ("images/" ++ fileName ++ ".png") hitomezashi
    where
        hitomezashi = genLft s x (head xs) + genTop s y (concat $ tail xs)
        y = length (head xs) * (s - 1) - (s - 2) -- get dims from ratio
        x = length (concat $ tail xs) * (s - 1) - (s - 2)

swap :: Eq a => a -> a -> [a] -> [a]
swap a b = map (\x -> if x == a then b else if x == b then a else x)

invert :: [[Bit]] -> [[Bit]]
invert [a,b] = [swap 0 1 a, swap 0 1 b]

toBinary :: [String] -> [[Bit]]
toBinary [a,b] = [bin a, bin b]
    where
        bin x = concatMap (reverse . binary) (concatMap (map ord) (words x))

binary :: Int -> [Int] 
binary 0 = [0]
binary n = let (q,r) = n `divMod` 2 in r : binary q

-- main

data Options = Options {
      optHelp :: Bool
    , optOutput :: String
    , optInput :: String 
    , optPattern :: [[Bit]]
    , optSize :: Int 
    , optInvert :: Bool 
    , optEncode :: [String]  
}

defaultOptions = Options {
      optHelp = False 
    , optOutput = "Hitomezashi Pattern.png"
    , optInput = ""
    , optPattern = [[]]
    , optSize = 4
    , optInvert = False 
    , optEncode = [""]
}

help :: IO ()
help = do
        putStrLn "Hitomezashi Maker"
        putStrLn "=================="
        putStrLn "Make various patterns and even encode messages!"
        putStrLn ""
        putStrLn "Usage:"
        putStrLn "-------"
        putStrLn "  -h : Display this help info"
        putStrLn "  -o : Specify the output file"
        putStrLn "  -f : Specify the input file (if available)"
        putStrLn ""
        putStrLn "  -H : Specify hitomezashi pattern"
        putStrLn "  -s : Specify the size of the lines (3,4 or 5, default 4)"
        putStrLn "  -i : Invert the hitomezashi"
        putStrLn "  -e : Encode a message in binary."
        putStrLn "  -d : !!! NOT IMPLEMENTED !!! Decode a message"
        putStrLn ""
        putStrLn "Hitomezashi Pattern Format:"
        putStrLn "----------------------------"
        putStrLn "  Patterns are made with a series of alternating lines starting from the left and top,"
        putStrLn "  and are represented by 1 and 0. 1 starts with a line, whereas 0 dosen't."
        putStrLn ""
        putStrLn "  eg. 1: |─ ─ ─ ─ ─|"
        putStrLn "      0: | ─ ─ ─ ─ |"
        putStrLn ""
        putStrLn "  To make a pattern, put a series of these into [], one for the left and one for the top"
        putStrLn ""
        putStrLn "  eg. [[1,0,1,0,1],[1,0,1,0,1]]  ( [[left pattern],[top pattern]] )"
        putStrLn ""
        putStrLn "  You can put this in a file and specify it with the -f option, or use it directly with the -H option"
        putStrLn "  or even encode a message with -e option ( [msgLeft,msgTop] )."
        putStrLn ""
        putStrLn "  Whatever it is you do, dont forget to have fun!"

main = do putStrLn "Hello! :)"