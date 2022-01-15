{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
-- hitomezashi patters can be interpreted as binary, so the pattern '- - - ' -> '101010'
-- it can also be interpreted using opposite rules, like odd and even numbers, and vowels and constanasnts
-- they can be represented as rulesets

-- a string list and a ruleset should be given in, and a pattern to come out

-- eg ['10101', '01010'] [1 -> '= or ||', 0 -> '.']
-- ie [top, left] [ruleset]

module Main where

import Data.Char
import Data.List
import Data.Data
import Data.ByteString as B
import qualified Graphics.Image as I
import Options.Applicative

type Image = I.Image I.VU I.YA Double

type Size = Int

type Len = Int

type Bit = Int

data Orientation = Horizontal | Vertical deriving (Show, Eq)

concatLR, concatTB :: [Image] -> Image
concatLR = foldl1 I.leftToRight
concatTB = foldl1 I.topToBottom

-- S CANNOT BE SMALLER THAN 3

genLn :: Orientation -> Size -> Len -> Bit -> Image
genLn Horizontal s len i =
  I.fromListsR I.VU $
    if i == 1
      then [take len $ cycle $ replicate s blk ++ replicate sp trns]
      else
        if i == 0
          then [take len $ trns : if last zlist == trns then init zlist else zlist] -- 0 lines have an exra space at the begin and end
          else [take len $ cycle $ replicate sp trns ++ replicate sp trns]
  where
    blk = I.PixelYA 0 1 -- black
    trns = I.PixelYA 0 0 -- transparent
    sp = s `mod` 3 + 1 -- spaces
    zlist = take len $ cycle $ replicate sp trns ++ replicate s blk
genLn Vertical s len i =
  I.transpose $
    I.fromListsR I.VU $
      if i == 1
        then [take len $ cycle $ replicate s blk ++ replicate sp trns]
        else
          if i == 0
            then [take len $ trns : if last zlist == trns then init zlist else zlist] -- 0 lines have an exra space at the begin and end
            else [take len $ cycle $ replicate sp trns ++ replicate sp trns]
  where
    blk = I.PixelYA 0 1 -- black
    trns = I.PixelYA 0 0 -- transparent
    sp = s `mod` 3 + 1 -- spaces
    zlist = take len $ cycle $ replicate sp trns ++ replicate s blk

genTop, genLft :: Size -> Len -> [Bit] -> Image
genTop s len xs = concatLR $ map (genLn Vertical s len) (intercalate (replicate sp 2) (map (: []) xs))
  where
    sp = s `mod` 3 + 1
genLft s len xs = concatTB $ map (genLn Horizontal s len) (intercalate (replicate sp 2) (map (: []) xs))
  where
    sp = s `mod` 3 + 1

swap :: Eq a => a -> a -> [a] -> [a]
swap a b = map (\x -> if x == a then b else if x == b then a else x)

invert :: [[Bit]] -> [[Bit]]
invert [a, b] = [swap 0 1 a, swap 0 1 b]

toBinary :: [String] -> [[Bit]]
toBinary [a, b] = [bin a, bin b]
  where
    bin x = concatMap (reverse . binary) (concatMap (map ord) (words x))

binary :: Int -> [Int]
binary 0 = [0]
binary n = let (q, r) = n `divMod` 2 in r : binary q

writeHitomezashi :: String -> Size -> [[Bit]] -> IO ()
writeHitomezashi fileName s xs = I.writeImage fileName hitomezashi
  where
    hitomezashi = genLft s x (head xs) + genTop s y (concat $ tail xs)
    y = length (head xs) * (s - 1) - (s - 2) -- get dims from ratio
    x = length (concat $ tail xs) * (s - 1) - (s - 2)
    
-- main

data Options = Options
  { optOutput :: String,
    optSize :: Int,
    optInvert :: Bool,
    optInput :: Input
  }

data Input = File String | Pattern [[Int]] | Encode [String] deriving (Typeable, Data)

getInputVal :: Input -> a
getInputVal (File n) = n
getInputVal (Pattern n) = n
getInputVal (Encode n) = n

fileInput :: Parser Input
fileInput = File <$> strOption
             ( long "input"
            <> short 'f'
            <> help "Specify's the input file with the pattern"
            <> metavar "FILE")

patternInput :: Parser Input
patternInput = Pattern <$> option patternReader
                 ( long "pattern"
                <> short 'p'
                <> help "Specify's the pattern"
                <> metavar "PATTERN")

encodeInput :: Parser Input
encodeInput = Encode <$> option strListReader
                 ( long "encode"
                <> short 'e'
                <> help "Encode a message in binary"
                <> metavar "STRING")

input :: Parser Input
input = fileInput <|> patternInput <|> encodeInput

patternReader :: ReadM [[Int]]
patternReader = do
    o <- str
    return (read o :: [[Int]])

strListReader :: ReadM [String]
strListReader = do
    o <- str
    return (read o :: [String])

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "output"
          <> short 'o'
          <> help "Specify's the output file (png)"
          <> metavar "FILE"
          <> value "out.png"
      )
    <*> option auto
      ( long "size"
          <> short 's'
          <> help "The size of the lines (3,4 or 5)"
          <> metavar "NUMBER"
          <> value 4
      )
    <*> switch
      ( long "invert"
          <> short 'i'
          <> help "Inverts the pattern (i.e. flips 0's to 1's and vice versa)"
      )
    <*> input

opts :: ParserInfo Options
opts = info (options <**> helper) (
  fullDesc
  <> progDesc "Make various patterns and even encode messages!"
  <> header "Hitomezashi Maker"
  <> footer "Hitomezashi Pattern Format:\n\
  \----------------------------\n\
  \Patterns are made with a series of alternating lines starting from the left and top,\n\
  \and are represented by 1 and 0. 1 starts with a line, whereas 0 dosen't.\n\n\
  \  eg. 1: |─ ─ ─ ─ ─|\n\
  \      0: | ─ ─ ─ ─ |\n\
  \To make a pattern, put a series of these into [], one for the left and one for the top\n\n\
  \eg. [[1,0,1,0,1],[1,0,1,0,1]]  ( [[left pattern],[top pattern]] )\n\n\
  \You can put this in a file and specify it with the -f option, or use it directly with the -H option\
  \or even encode a message with -e option ( [msgLeft,msgTop] ).\n\n\
  \Whatever it is you do, dont forget to have fun!\n"
  )

writeHitomezashiWrapper :: Options -> IO ()
writeHitomezashiWrapper Options{ .. }
  | toConstr optInput == toConstr Pattern = writeHitomezashi optOutput optSize (getInputVal t)
  | toConstr optInput == toConstr Encode  = writeHitomezashi optOutput optSize (toBinary $ getInputVal t)
  | toConstr optInput == toConstr File    = 