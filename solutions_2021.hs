-- Solutions to Advent of Code 2021, through puzzle 22a.
--
-- By Truman Collins
-- December 1, 2021 to February 11, 2022
--
-- I have organized all of the solutions to these problems that I implemented in a single source
-- file, which is not best practice. As this is a fun project and the problems are generally fairly
-- straightforward, I did it this way to easily share code and run, time, and check all of the
-- problems at once. I typically define any relevant types with the problem prefix in the name, then
-- have an IO function that will read the input, compute the two answers, and return them out to a
-- calling function that checks for the correct answer and times the two solutions. I have generally
-- put all of the functions related to a particular problem in the 'where' clause at the bottom of
-- the main IO function. Again, not best practice for a real project, but it keeps functionality
-- contained for each puzzle.
-- Note that in the timings for the two answers for a particular puzzle. The first one includes
-- parsing, and common calculation, and any computation done before final error checks in the IO
-- code.

{-# Language TupleSections #-}
{-# Language BinaryLiterals #-}

{-# Language ScopedTypeVariables #-}
{-# Language DuplicateRecordFields #-}

import Data.List
import Data.Char
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Function
import qualified Data.Bifunctor as BF
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified Data.Graph.Inductive.Graph as GR
import qualified Data.Graph.Inductive.PatriciaTree as GRPT
import qualified Data.Graph.Inductive.Query.SP as GRSP
import Control.Applicative
import Control.Monad
import System.IO (hFlush, stdout)
import System.Clock
import Text.Printf
import Parsers

-- The amount of space to allow for a result printed.

answerSpace :: Int
answerSpace = 14

-- Helper functions for time calculations.

convertTimeToDouble :: TimeSpec -> Double
convertTimeToDouble tm = fromIntegral (sec tm) + fromIntegral (nsec tm) / 1.0e9

computeElapsedTime :: (TimeSpec, TimeSpec) -> Double
computeElapsedTime (startTime, endTime)
  = convertTimeToDouble endTime - convertTimeToDouble startTime

computeCheckAndPrint1 :: (Integral a, Show a, PrintfArg a) => IO a -> String -> a -> IO ()
computeCheckAndPrint1 puzzleFn puzzleNamePre correctAns = do

  -- Time the computations and make sure the calculation is done before the end time is read.

  startTime <- getTime Realtime
  result    <- puzzleFn
  endTime   <- result `seq` getTime Realtime

  -- Print and label the result, and left pad the answer so the timings line up.

  let diff      = computeElapsedTime (startTime, endTime)
      diffStr   = printf "%0.5f sec" diff
      resultStr = genResultString (result, correctAns, diffStr)

  -- Print out the results for parts A and B.

  outputResult "a: " resultStr
  hFlush stdout

  where

    genResultString (result, corrAns, tDiffStr) = resultStr
      where
        resultStr = if result == corrAns
                    then ansPadded ++ "  (" ++ tDiffStr ++ ")"
                    else printf "Error: expected %d, but computed %d (%s)" corrAns result tDiffStr
        ansPadded = foldl' (\acc _ -> ' ' : acc) ansStr [(length ansStr)..answerSpace]
        ansStr = show result

    outputResult aOrBStr resultStr =
      putStrLn $ mconcat ["Result ", puzzleNamePre, aOrBStr, resultStr]

-- Check validity, based on known answer, and time running the solutions of two integer answers.

computeCheckAndPrint2 :: (Integral a, Show a, PrintfArg a, Integral b, Show b, PrintfArg b)
                         => IO (a, b) -> String -> (a, b) -> IO ()
computeCheckAndPrint2 puzzleFn puzzleNamePre (correctAnsA, correctAnsB) = do

  -- Time the computations and make sure the calculation is done before the end time is read.

  startTimeA <- getTime Realtime
  (resultA, resultB) <- puzzleFn
  endTimeA <- resultA `seq` getTime Realtime
  startTimeB <- getTime Realtime
  endTimeB <- resultB `seq` getTime Realtime

  -- Print and label the result, and left pad the answer so the timings line up.

  let [diffA, diffB] = map computeElapsedTime [(startTimeA, endTimeA), (startTimeB, endTimeB)]
      [diffStrA, diffStrB] = map (printf "%0.5f sec") [diffA, diffB]
      resultStrA = genResultString (resultA, correctAnsA, diffStrA)
      resultStrB = genResultString (resultB, correctAnsB, diffStrB)

  -- Print out the results for parts A and B.

  outputResult "a: " resultStrA
  outputResult "b: " resultStrB
  hFlush stdout

  where

    genResultString (result, corrAns, tDiffStr) = resultStr
      where
        resultStr = if result == corrAns
                    then ansPadded ++ "  (" ++ tDiffStr ++ ")"
                    else printf "Error: expected %d, but computed %d (%s)" corrAns result tDiffStr
        ansPadded = foldl' (\acc _ -> ' ' : acc) ansStr [(length ansStr)..answerSpace]
        ansStr = show result

    outputResult aOrBStr resultStr =
      putStrLn $ mconcat ["Result ", puzzleNamePre, aOrBStr, resultStr]

computeCheckAndPrint2IS :: (Integral a, Show a, PrintfArg a)
                             => IO (a, String) -> String -> (a, String) -> IO ()
computeCheckAndPrint2IS puzzleFn puzzleNamePre (correctAnsA, correctAnsB) = do

  -- Time the computations and make sure the calculation is done before the end time is read.

  startTimeA <- getTime Realtime
  (resultA, resultB) <- puzzleFn
  endTimeA   <- resultA `seq` getTime Realtime
  startTimeB <- getTime Realtime
  endTimeB   <- last resultB `seq` getTime Realtime

  -- Print and label the result, and left pad the answer so the timings line up.

  let [diffA, diffB] = map computeElapsedTime [(startTimeA, endTimeA), (startTimeB, endTimeB)]
      [diffStrA, diffStrB] = map (printf "%0.5f sec") [diffA, diffB]
      resultStrA = genResultString (resultA, correctAnsA, diffStrA)
      resultStrB = genResultString_S (resultB, correctAnsB, diffStrB)

  -- Print out the results for parts A and B.

  outputResult "a: " resultStrA
  outputResult "b: " resultStrB
  hFlush stdout

  where

    genResultString (result, corrAns, tDiffStr) = resultStr
      where
        resultStr = if result == corrAns
                    then ansPadded ++ "  (" ++ tDiffStr ++ ")"
                    else printf "Error: expected %d, but computed %d (%s)" corrAns result tDiffStr
        ansPadded = foldl' (\acc _ -> ' ' : acc) ansStr [(length ansStr)..answerSpace]
        ansStr = show result

    genResultString_S (result, corrAns, tDiffStr) = resultStr
      where
        resultStr = if result == corrAns
                    then ansPadded ++ "  (" ++ tDiffStr ++ ")"
                    else printf "Error: expected %s, but computed %s (%s)" corrAns result tDiffStr
        ansPadded = foldl' (\acc _ -> ' ' : acc) ansStr [(length ansStr)..answerSpace]
        ansStr = result

    outputResult aOrBStr resultStr =
      putStrLn $ mconcat ["Result ", puzzleNamePre, aOrBStr, resultStr]

--
-- Some utility functions used by more than one puzzle.
--

-- Convert a list of int lists to a two dimensional array, with (0, 0) being the first int in the
-- first list, and progressing positively for both X and Y from there. The int lists must all be of
-- the same length.

convertDoubleListOfIntsTo2DArray :: [[Int]] -> UA.Array (Int, Int) Int
convertDoubleListOfIntsTo2DArray xss = UA.array ((0, 0), (colLimit, rowLimit)) arrInitList
  where
    arrInitList = zip [(x, y) | y <- [0..rowLimit], x <- [0..colLimit]] (concat xss)
    rowLimit = length xss - 1
    colLimit = (if null xss then 0 else (length . head) xss) - 1

-- Read an array of digits from the given file for the given puzzle and return the values in a
-- two-dimensional array of digits. If flipYOrder is set, then the (0, 0) element will be the bottom
-- left textually, whereas if it is false, then the (0, 0) element will be the top left textually.

readArrayOfDigits :: Bool -> String -> String -> IO (UA.Array (Int, Int) Int)
readArrayOfDigits flipYOrder fileName puzzleID = do
  parseRes <- fmap (map (parse parseDigitLine) . lines) (readFile fileName)

  when (badParseOfInputLines parseRes)
       (ioError $ userError ("Parse error in input for puzzle " ++ puzzleID ++ "."))

  let valListsAsRead = map (fst . head) parseRes
      valListsForArr = if flipYOrder then reverse valListsAsRead else valListsAsRead

      -- Here we have the array of heights, and from it compute a list of the low point
      -- coordinates. Both answers will come from this list of low points.

      valArr  = convertDoubleListOfIntsTo2DArray valListsForArr
  return valArr

  where

    -- Parse all of the digits in the string as individual ints.

    parseDigitLine :: Parser [Int]
    parseDigitLine = do
      inString <- many digit
      return (fmap digitToInt inString)

-- Given an (x, y) coordinate, return the four coordinates one step away horizontally or vertically.

nextDoorHV :: (Ord a, Num a) => (a, a) -> [(a, a)]
nextDoorHV coord = [BF.first (+ 1), BF.second (+ 1), BF.first sub1, BF.second sub1] <*> [coord]
  where sub1 = subtract 1

-- Given an (x, y) coordinate, return the eight locations around it.

nextDoorHVD :: (Ord a, Num a) => (a, a) -> [(a, a)]
nextDoorHVD (x, y) = drop 1 [(x', y') | x' <- [x, x - 1, x + 1], y' <- [y, y - 1, y + 1]]

-- Return true if the given (x, y) value falls in the range of ((lowX, lowY), (highX, highY)).

inBoundsXY :: Ord a => ((a, a), (a, a)) -> (a, a) -> Bool
inBoundsXY ((lowX, lowY), (highX, highY)) (x, y) = x >= lowX && x <= highX && y >= lowY && y <= highY

--
-- Functions for the individual puzzles.
--

--
-- Code for Puzzle 1.
--

puzzle_01 :: IO (Int, Int)
puzzle_01 = do
  depths :: [Int] <- fmap (map read . lines) (readFile "puzzle_01.inp")

  let deeperCountA = countLargerThanLast depths
      deeperCountB = countLargerThanLast sumOfThrees
      sumOfThrees  = zipWith3 (\x y z -> x + y + z) depths (drop 1 depths) (drop 2 depths)

  return (deeperCountA, deeperCountB)

  where

    countLargerThanLast :: (Num a, Ord a) => [a] -> Int
    countLargerThanLast [] = 0
    countLargerThanLast xs = (length . filter (> 0)) (zipWith (-) (drop 1 xs) xs)

data P02Direction = Forward Int | Down Int | Up Int deriving Show

data P02Loc = P02Loc { _horiz_r :: Int
                     , _depth_r :: Int
                     , _aim_r   :: Int
                     }

--
-- Code for Puzzle 2.
--

puzzle_02 :: IO (Int, Int)
puzzle_02 = do
  parserRes <- fmap (map (parse parseDirection) . lines) (readFile "puzzle_02.inp")

  when (badParseOfInputLines parserRes)
       (ioError $ userError "Parse error in input for puzzle 02.")

  let moves = map (fst . head) parserRes

      -- Compute the final horizontal and depth values based on the rules for both parts.

      (P02Loc hValA dValA _) = foldl' makeMoveA (P02Loc 0 0 0) moves
      (P02Loc hValB dValB _) = foldl' makeMoveB (P02Loc 0 0 0) moves

  return (hValA * dValA, hValB * dValB)

  where

    -- The move rules for part A.

    makeMoveA :: P02Loc -> P02Direction -> P02Loc
    makeMoveA (P02Loc h d a) (Forward n) = let newH = h + n in newH `seq` P02Loc newH d a
    makeMoveA (P02Loc h d a) (Down n)    = let newD = d + n in newD `seq` P02Loc h newD a
    makeMoveA (P02Loc h d a) (Up n)      = let newD = d - n in newD `seq` P02Loc h newD a

    -- The move rules for part B.

    makeMoveB :: P02Loc -> P02Direction -> P02Loc
    makeMoveB (P02Loc h d a) (Forward n) = let newH = h + n
                                               newD = newH `seq` d + a * n
                                           in  newD `seq` P02Loc newH newD a
    makeMoveB (P02Loc h d a) (Down n)    = let newA = a + n in newA `seq` P02Loc h d newA
    makeMoveB (P02Loc h d a) (Up n)      = let newA = a - n in newA `seq` P02Loc h d newA

    -- This function parses the direction. I have used fmap here, and want to explain that.  In the
    -- first possibility, Forward, we expect to parse an integer, so make this parser call, which
    -- will return an integer in a monad. the straightforward way to write this would be:
    --   val <- integer
    --   return (Forward val)
    -- Instead, I fmap the construction function Forward onto this monadic value, which results in
    -- the integer wrapped in a Forward. Because this is the last line of the do block, there is a
    -- return of this value, which gets us to the same result. I have done this for Down and Up as
    -- well, and this change was recommended by hlint.
    
    parseDirection :: Parser P02Direction
    parseDirection = do
                       _ <- symbol "forward"
                       fmap Forward integer
                     <|> do
                       _ <- symbol "down"
                       fmap Down integer
                     <|> do
                       _ <- symbol "up"
                       fmap Up integer

--
-- Code for Puzzle 3.
--

puzzle_03 :: IO (Int, Int)
puzzle_03 = do
  inputStrs <- fmap lines (readFile "puzzle_03.inp")

  -- Data needed for part A of this puzzle.

  let entryCount = length inputStrs
      entryLen   = length (head inputStrs)
      target     = entryCount `quot` 2
      oneColCnts = foldl' addOnesToCounts (replicate entryLen 0) inputStrs
      gammaVal   = foldl' (addBit (> target)) 0 oneColCnts
      epsilonVal = foldl' (addBit (<= target)) 0 oneColCnts

  -- Data needed for part B of this puzzle.

  let bitList      = map (convToInt 0) inputStrs
      (_, oxyList) = (head . dropWhile moreToDo . iterate (winnow (>=))) (entryLen - 1, bitList)
      oxyGenRate
        | length oxyList == 1 = head oxyList
        | otherwise = error "Bad Oxygen rate result."
      (_, co2List) = (head . dropWhile moreToDo . iterate (winnow (<))) (entryLen - 1, bitList)
      co2ScrubRate
        | length co2List == 1 = head co2List
        | otherwise = error "Bad CO2 rate result."

  return (gammaVal * epsilonVal, oxyGenRate * co2ScrubRate)

  where

    addOnesToCounts :: [Int] -> String -> [Int]
    addOnesToCounts = zipWith incIf1

    -- Add one to the first parameter if the second is a '1'.

    incIf1 :: Int -> Char -> Int
    incIf1 c '0' = c
    incIf1 c '1' = c + 1
    incIf1 _ _ = error "Invalid character in incIf1."

    -- Multiply by 2, and add a bit to the 1's location if needed.

    addBit :: (Int -> Bool) -> Int -> Int -> Int
    addBit fn acc c = acc * 2 + if fn c then 1 else 0

    -- Convert a string of 1s and 0s to an int.

    convToInt :: Int -> String -> Int
    convToInt acc [] = acc
    convToInt acc (x : xs) = let shifted = acc * 2
                                 newAcc = if x == '0' then shifted else shifted + 1
                             in  newAcc `seq` convToInt newAcc xs

    -- Returns true if more winnowing needed.

    moreToDo :: (Int, [Int]) -> Bool
    moreToDo (cnt, xs) = cnt >= 0 && not (lenLT2 xs)

    -- Return true if the length of the list is less than 2.

    lenLT2 :: [a] -> Bool
    lenLT2 [] = True
    lenLT2 [_] = True
    lenLT2 _ = False

    winnow :: (Int -> Int -> Bool) -> (Int, [Int]) -> (Int, [Int])
    winnow fn (shiftCount, values) = (shiftCount - 1, winnowedList)
      where
        winnowedList = if fn oneCount zeroCount then filter isBitSet values
                       else filter (not . isBitSet) values 
        oneCount   = (length . filter id . map isBitSet) values
        zeroCount  = valueCount - oneCount
        valueCount = length values
        isBitSet   = flip testBit shiftCount

--
-- Code for Puzzle 4.
--

type P04NumberMap = M.Map Int [(Int, (Int, Int))]
type P04BingoCard = UA.Array (Int, Int) Int
type P04BingoIndex = (Int, Int)
type P04BingoIndices = [P04BingoIndex]
type P04BingoMask = Int
type P04BingoCardStates = UA.Array Int P04BingoMask
type P04WinningCardsAndMask = (Int, P04BingoMask)

puzzle_04 :: IO (Int, Int)
puzzle_04 = do
  parseRes <- fmap (parse (readBingoData cardSize)) (readFile "puzzle_04.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 4.")

  let (drawnNumbers, cardData) = (fst . head) parseRes
      cardCount = length cardData

      -- The array of bingo cards as read. These never change. Numbers called are tracked as bits in
      -- gameStatesAfterMoves.

      bingoCardArrays :: A.Array Int P04BingoCard
      bingoCardArrays = A.array (0, cardCount - 1) $ (zip [0..] . map convTo2DArray) cardData

      -- For each number in any bingo card, holds the list of locations that number is, noted by
      -- bingo card index and coordinates in the card.

      numberLocs :: P04NumberMap
      numberLocs = foldl' noteBingoNumbersAll M.empty (A.assocs bingoCardArrays)

      -- Here we have an empty set of ints, used for bit vectors, indexed by card number and
      -- representing the marked locations on each board, when filled in.

      emptyCardMasks :: P04BingoCardStates
      emptyCardMasks = UA.array (0, cardCount - 1) [(x, 0) | x <- [0..(cardCount - 1)]]

      -- Each item in the list has the called number, and the bit vectors for each card representing
      -- the squares marked after this and all prior called numbers.

      gameStatesAfterMoves :: [(Int, P04BingoCardStates)]
      gameStatesAfterMoves = scanl (updateForNumber numberLocs) (-1, emptyCardMasks) drawnNumbers

      -- The list of states starting with the first called number and ending with the last. The
      -- first number in each tuple is the called number, the second is a list of the boards that
      -- won after that number was called along with the bit mask indicating the marked locations on
      -- that board. The third item is the boards (by index) remaining after this number was called,
      -- that are still active boards.

      wonAndRemaining :: [(Int, [P04WinningCardsAndMask], [Int])]
      wonAndRemaining = dropWhile (\(cn, _, _) -> cn == -1)
                        $ scanl winsAndRemaining (-1, [], [0..(cardCount - 1)]) gameStatesAfterMoves

      -- Find the first winning card. This is returned as an entry in a list, so error checking can
      -- be done to make sure there really is one.

      firstWins :: [(Int, [P04WinningCardsAndMask])]
      firstWins = (take 1 . dropWhile (null . snd) . map (\(x, y, _) -> (x, y))) wonAndRemaining

      -- Get rid of all number calls until there are no more cards left. That entry should contain a
      -- single board that won. Again, return a list in case there is a card that never wins. Error
      -- checking will be done later.

      lastWins :: [(Int, [P04WinningCardsAndMask])]
      lastWins = (take 1 . map (\(x, y, _) -> (x, y))
                  . dropWhile (\(_, _, xs) -> (not . null) xs)) wonAndRemaining

  -- Make sure there was at least one first and last win.

  when (null firstWins)
       (ioError $ userError "No solutions found for puzzle 4a.")
  when (null lastWins)
       (ioError $ userError "No solutions found for puzzle 4b.")

  let (firstWinCallNumber, firstWinBoards) = head firstWins
      (lastWinCallNumber, lastWinBoards)   = head lastWins

  -- Now check to make sure that there is only a single board winning in each of the first and last wins.

  unless ((null . tail) firstWinBoards)
       (ioError $ userError "More than one solution found for puzzle 4a.")

  unless ((null . tail) lastWinBoards)
       (ioError $ userError "More than one solution found for puzzle 4b.")

  let (firstBoardNumber, firstMask) = head firstWinBoards
      (lastBoardNumber, lastMask)   = head lastWinBoards
      firstWinResult = ((* firstWinCallNumber) . sum
                        . foldr (nonUsedNumber firstBoardNumber firstMask) []) allCardIndices
      lastWinResult = ((* lastWinCallNumber) . sum
                        . foldr (nonUsedNumber lastBoardNumber lastMask) []) allCardIndices

      -- Include the number on the bingo card corresponding to this board and index if it has not
      -- been called.

      nonUsedNumber :: Int -> P04BingoMask -> P04BingoIndex -> [Int] -> [Int]
      nonUsedNumber bdInd msk ind acc
        | indexCalled cardSize msk ind = acc
        | otherwise = ((bingoCardArrays A.! bdInd) UA.! ind) : acc

  return (firstWinResult, lastWinResult)

  where

    cardSize = 5
    bingoMasksAndInds = genBingoMasksAndIndices

    -- Given a new called number, update the array of bit masks representing the numbers called for
    -- each card.

    updateForNumber :: P04NumberMap -> (Int, P04BingoCardStates) -> Int -> (Int, P04BingoCardStates)
    updateForNumber locMap (_, currStates) currNumber

      -- If this number doesn't appear in any of the bingo cards, then just return the prior state.

      | isNothing locsM = (currNumber, currStates)

      -- We want to mark any occurrences of this number in any bingo boards as found, which is done
      -- with a bit mask for each card.

      | otherwise = (currNumber, UA.accum (markBitForIndex cardSize) currStates (fromJust locsM))
      where
        locsM = M.lookup currNumber locMap

    -- Combine the current call number and state array with the cards that are still active to
    -- return any winning cards after this call number and the new set of still-active cards.

    winsAndRemaining :: (Int, [P04WinningCardsAndMask], [Int]) -> (Int, P04BingoCardStates)
                        -> (Int, [P04WinningCardsAndMask], [Int])
    winsAndRemaining (_, _, activeCards) (callNumber, stateArray)
      = (callNumber, winsAndMasks, stillActiveCards)
      where
        winsAndMasks = (filter hasBingo . map (\i -> (i, stateArray UA.! i))) activeCards
        stillActiveCards = removeValsSorted (map fst winsAndMasks) activeCards

        -- Return true if the given bit mask of numbers called for a card has a bingo.

        hasBingo :: P04WinningCardsAndMask -> Bool
        hasBingo (_, mask) = any ((\msc -> msc == msc .&. mask)  . fst) bingoMasksAndInds

        -- Remove the items in the first list from the second. Lists must be sorted.

        removeValsSorted :: (Ord a) => [a] -> [a] -> [a]
        removeValsSorted [] ys = ys
        removeValsSorted _ [] = []
        removeValsSorted xss@(x : xs) yss@(y : ys)
          | x < y = removeValsSorted xs yss
          | x > y = y : removeValsSorted xss ys
          | otherwise = removeValsSorted xss ys

    -- Convert a bingo card in list form to a two-dimensional array with the lower left corner at
    -- (0, 0).

    convTo2DArray :: [[Int]] -> P04BingoCard
    convTo2DArray entries = UA.array ((0, 0), (4, 4)) arrayInitList
      where
        arrayInitList = (concatMap addIndexes . zip [4,3..0]) entries
        addIndexes :: (Int, [Int]) -> [((Int, Int), Int)]
        addIndexes (y, nums) = zipWith (\x n -> ((x, y), n)) [0..] nums

    -- Insert all of the card location data into the map for this card.

    noteBingoNumbersAll :: P04NumberMap -> (Int, P04BingoCard) -> P04NumberMap
    noteBingoNumbersAll accMap (cardIndex, cardArr) = newAccMap
      where
        newAccMap = foldl' noteBingoNumbersCard accMap (UA.assocs cardArr)

        -- Walk through the values in this card, inserting the location data in the map.

        noteBingoNumbersCard :: P04NumberMap -> ((Int, Int), Int) -> P04NumberMap
        noteBingoNumbersCard currMap (xyIndices, value) = newMap
          where
            newMap = M.insertWith (\[x] acc -> x : acc) value [(cardIndex, xyIndices)] currMap

    -- Some useful ranges and lists.

    sideRange = [0..(cardSize - 1)]
    allCardIndices = [(x, y) | x <- sideRange, y <- sideRange]
    fullRowIndices = map ((\ xs y -> map (, y) xs) sideRange) sideRange
    fullColIndices = map (map (\(x, y) -> (y, x))) fullRowIndices

    -- Generate bit masks and corresponding indices in the bingo board for each valid bingo. These
    -- will be used by all bingo boards.

    genBingoMasksAndIndices :: [(P04BingoMask, P04BingoIndices)]
    genBingoMasksAndIndices = map genBitMasks (fullRowIndices ++ fullColIndices)
      where
        genBitMasks :: P04BingoIndices -> (P04BingoMask, P04BingoIndices)
        genBitMasks indices = (foldl' (markBitForIndex cardSize) 0 indices, indices)

    parseBingoCards :: Int -> Parser [[[Int]]]
    parseBingoCards cardSz = do many (parseNItems cardSz oneLineInCard)
      where

        -- Parse one full line of cardSize number in a bingo card.

        oneLineInCard :: Parser [Int]
        oneLineInCard = do parseNItems cardSz oneNumberInCard

        -- Parse one number in a bingo card.

        oneNumberInCard :: Parser Int
        oneNumberInCard = do
          _ <- space
          natural

    -- Parse the whole input. This problem calls for a card size of 5x5, but this will work with
    -- other square sizes.

    readBingoData :: Int -> Parser ([Int], [[[Int]]])
    readBingoData cardSz = do
      calledNumbers <- cslOfNats
      _ <- space
      listOfBingoCards <- parseBingoCards cardSz
      return (calledNumbers, listOfBingoCards)

    -- Used to set and access card information from the bingo mask.

    markBitForIndex :: Int -> P04BingoMask -> P04BingoIndex -> P04BingoMask
    markBitForIndex cardSz  mask (x, y) = mask .|. shiftL 0b1 (y * cardSz + x)
    indexCalled :: Int -> P04BingoMask -> P04BingoIndex -> Bool
    indexCalled cardSz mask (x, y) = mask .&. shiftL 0b1 (y * cardSz + x) /= 0

--
-- Code for Puzzle 5.
--

type P05Coordinate = (Int, Int)
type P05VentLine = (P05Coordinate, P05Coordinate)
type P05VentLineItem = (P05LineDir, [P05Coordinate])

data P05LineDir = Horizontal | Vertical | Diagonal deriving (Eq, Show)

puzzle_05 :: IO (Int, Int)
puzzle_05 = do
  parseRes <- fmap (map (parse parseVentLine) . lines) (readFile "puzzle_05.inp")

  when (badParseOfInputLines parseRes)
       (ioError $ userError "Parse error in input for puzzle 5.")

  let ventLines = map (fst . head) parseRes
      (allXs, allYs) = foldr splitXsAndYs ([], []) ventLines
      minCoords = (minimum allXs, minimum allYs)
      maxCoords = (maximum allXs, maximum allYs)
      allVents  = map convToVentLines ventLines

      -- This is an array holding all of the horizontal and vertical vent lines.

      ventLineArrA :: UA.Array P05Coordinate Int
      ventLineArrA = UA.accumArray (+) 0 (minCoords, maxCoords)
                       (zip ((concatMap snd . filter ((/= Diagonal) . fst)) allVents) (repeat 1))

      -- This is an array holding all of the vent lines, including diagonals.

      ventLineArrB :: UA.Array P05Coordinate Int
      ventLineArrB = UA.accumArray (+) 0 (minCoords, maxCoords)
                       (zip (concatMap snd allVents) (repeat 1))

      -- The answers to parts A and B.

      partAOverlapCellCount = (length . filter (> 1)) (UA.elems ventLineArrA)
      partBOverlapCellCount = (length . filter (> 1)) (UA.elems ventLineArrB)

  return (partAOverlapCellCount, partBOverlapCellCount)

  where

    -- Convert a vent pair to a list of coordinates making up that line, and include whether it is
    -- horizontal, vertical, or diagonal.

    convToVentLines :: P05VentLine -> P05VentLineItem
    convToVentLines ((x1, y1), (x2, y2))
      | x1 == x2  = (Horizontal, [(x1, y) | y <- [minY..maxY]])
      | y1 == y2  = (Vertical,   [(x, y1) | x <- [minX..maxX]])
      | otherwise = if (x1 < x2 && y1 < y2) || (x1 > x2 && y1 > y2)
                    then (Diagonal, zip [minX..maxX] [minY..maxY])
                    else (Diagonal, zip [minX..maxX] [maxY,(maxY - 1)..minY])
      where
        minX = min x1 x2
        maxX = max x1 x2
        minY = min y1 y2
        maxY = max y1 y2

    -- Separate out the x and y values that are the endpoints of vent lines into two separate lists.

    splitXsAndYs :: P05VentLine -> ([Int], [Int]) -> ([Int], [Int])
    splitXsAndYs ((x1, y1), (x2, y2)) (xs, ys) = (x1 : x2 : xs, y1 : y2 : ys)

    -- Parse a single line indicating a vent line.

    parseVentLine :: Parser ((Int, Int), (Int, Int))
    parseVentLine = do
      pair1 <- parseTwoCSNats
      _  <- space
      _  <- symbol "->"
      pair2 <- parseTwoCSNats
      _ <- space
      return (pair1, pair2)

    -- Parse two natural numbers separated by a comma, and return as a tuple.

    parseTwoCSNats :: Parser P05Coordinate
    parseTwoCSNats = do
      _ <- space
      x <- natural
      _ <- symbol ","
      y <- natural
      return (x, y)

--
-- Code for Puzzle 6.
--

type P06FishDayArr = UA.Array Int Int64

puzzle_06 :: IO (Int, Int64)
puzzle_06 = do
  parseRes <- fmap (parse readLanternfishTimes) (readFile "puzzle_06.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 6.")

  -- Use a list to keep track of each individual lanternfish and work through 80 iterations. Note
  -- that using an array to keep counts of the fish at each day is much more efficient. I left it
  -- this way for part 1 for comparison.

  let initFishTimes = (fst . head) parseRes
      resultA = (length . (!! max 0 daysToRunA) . iterate ageFishOneDayAndReprod) initFishTimes

  -- For the second part of this problem, the number of fish is much too large to do the same way,
  -- so keep track of the number of fish at each time in the cycle.

  let fishDayArray = UA.accumArray (+) 0 (0, 8) (map (, 1) initFishTimes)
      resultB = (sum . UA.elems . (!! max 0 daysToRunB) . iterate ageFishOneDayArr) fishDayArray
  
  return (resultA, resultB)

  where

    -- Days to go before couting.

    daysToRunA = 80
    daysToRunB = 256

    -- Age the list of fish one day, and if at 0, reset back to 6 and produce an offspring at 8.

    ageFishOneDayAndReprod :: [Int] -> [Int]
    ageFishOneDayAndReprod = foldr decAndPropagate []
      where
        decAndPropagate age acc
          | age == 0 = 6 : 8 : acc
          | otherwise = (age - 1) : acc

    -- Age the array of fish one day, counting all fish associated with each index together.

    ageFishOneDayArr :: P06FishDayArr -> P06FishDayArr
    ageFishOneDayArr currArr = UA.accumArray (+) 0 (0, 8) newFishDayCounts
      where
        newFishDayCounts = foldr decAndPropagate [] (UA.assocs currArr)
        decAndPropagate (age, count) acc
          | age == 0 = (6, count) : (8, count) : acc
          | otherwise = (age - 1, count) : acc

    -- The input is just a comma-separated list of natural numbers.

    readLanternfishTimes :: Parser [Int]
    readLanternfishTimes = cslOfNats

--
-- Code for Puzzle 7.
--

puzzle_07 :: IO (Int, Int)
puzzle_07 = do
  parseRes <- fmap (parse readCrabHorizLocs) (readFile "puzzle_07.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 6.")

  -- For the first part, the best location is the median of the data. The second part is harder, and
  -- I ended up just scanning a range for the lowest fuel-using location, but I'm sure there is a
  -- better way, perhaps using Newton's method.

  let crabHorizLocs = (fst . head) parseRes
      count = length crabHorizLocs
      crabHorizSorted = sort crabHorizLocs
      bestCrabPos = crabHorizSorted !! (count `quot` 2)
      totalFuel = computeSumToPointA crabHorizSorted bestCrabPos
      trialsB = map (computeSumToPointB crabHorizLocs) [400..600]
      (bestB, _) = minimum trialsB

  return (totalFuel, bestB)

  where

    -- Compute the fuel cost for all crabs to move to the given location, for part 1.

    computeSumToPointA :: [Int] -> Int -> Int
    computeSumToPointA xs pt = sum $ map (\x -> abs (x - pt)) xs

    -- Compute the fuel cost for all crabs to move to the given location, for part 2.

    computeSumToPointB :: [Int] -> Int -> (Int, Int)
    computeSumToPointB xs pt = (sum $ map compCost xs, pt)
      where
        compCost x = let dist = abs (x - pt)
                     in  dist * (dist + 1) `quot` 2

    -- The input is just a comma-separated list of natural numbers.

    readCrabHorizLocs :: Parser [Int]
    readCrabHorizLocs = cslOfNats

--
-- Code for Puzzle 8.
--

-- Determine for each input line the mapping of segments to the standard set, then decode the final
-- digits. We store sets of segments using a set, and that is plenty fast, but it would be more
-- efficient to store them as bits in an Int, or even Int16.

type P08SignalsForOutput = ([String], [String])
type P08PossSet = S.Set Char
type P08SegmentSet = S.Set Char
type P08ValidOptions = A.Array Char P08PossSet
type P08Correspondence = A.Array Char Char

puzzle_08 :: IO (Int, Int)
puzzle_08 = do
  parseRes <- fmap (map (parse parseSignalPatterns) . lines) (readFile "puzzle_08.inp")

  when (badParseOfInputLines parseRes)
       (ioError $ userError "Parse error in input for puzzle 8.")

  let signalPatterns = map (fst . head) parseRes
      uniqueDigitCount = (length . filter uniqueDigLength . concatMap snd) signalPatterns
      resultB = (sum . map decodeDigits) signalPatterns

  return (uniqueDigitCount, resultB)

  where

    -- Decode the given digit code strings into an int, given the sample strings for the 10 digits.

    decodeDigits :: ([String], [String]) -> Int
    decodeDigits (samples, codes) = foldl' (\acc d -> acc * 10 + d) 0 digits
      where
        digits = map decodeAndGetDigit codes
        correspondenceArr = deduceConnections samples

        decodeAndGetDigit :: String -> Int
        decodeAndGetDigit code
          | isNothing digitM = error ("Bad lookup in decodeAndGetDigit: " ++ show segments
                                      ++ " orig: " ++ show code)
          | otherwise = fromJust digitM
          where
            digitM = M.lookup (S.fromList segments) segmentsToDigMap
            segments = map (correspondenceArr A.!) code

    -- Given a set of sample strings for the 10 digits, deduce the correspondence to standard
    -- segments, and return a conversion array.

    deduceConnections :: [String] -> P08Correspondence
    deduceConnections samples
      | goodCorrespondence = correspondenceArr
      | otherwise = error "Non-unique correspondence."
      where
        correspondenceArr = (A.array ('a', 'g') . zip ['a'..'g'] . map (head . S.elems) . A.elems)
                            narrowedOptions
        goodCorrespondence = (all ((== 1) . S.size) . A.elems) narrowedOptions
        narrowedOptions = foldl' narrowChoicesGroup initialAllPossArr samplesGroupedLen
        samplesGroupedLen = (map (map snd) . groupBy ((==) `on` fst) . map (\xs -> (length xs, xs)))
                            samples
        initialAllPossArr = A.array ('a', 'g') (zip ['a'..'g'] (repeat allPoss))
        allPoss = S.fromList ['a'..'g']

    -- Given a list of digits of the same number of segments to display, find the common segments
    -- among all of them and use these to narrow the possible valid mappings.

    narrowChoicesGroup :: P08ValidOptions -> [String] -> P08ValidOptions
    narrowChoicesGroup validOptArr samples
      | lenSamp == 2 = updateSets ['c', 'f'] ['a', 'b', 'd', 'e', 'g']
      | lenSamp == 3 = updateSets ['a','c','f'] ['b','d','e','g']
      | lenSamp == 4 = updateSets ['b','c','d','f'] ['a','e','g']
      | lenSamp == 5 = updateSets ['a','d','g'] ['b','c','e','f']
      | lenSamp == 6 = updateSets ['a','b','f','g'] ['c','d','e']

      -- If this wasn't of lengths 2 through 6, then either we won't see that length or in the case
      -- of 7, we can't learn anything from it because all segments are lit.

      | otherwise = validOptArr
      where

        -- Take the one or more strings, and find the common characters among them.  Depending on
        -- the number of characters in the original strings, we can determine which segments these
        -- correspond to. To find the length, we take the length of the first sample in the list, as
        -- the list of common characters may be shorter.

        sampleOn  = (S.elems . foldl1' S.intersection . map S.fromList) samples
        sampleOff = removeThese sampleOn ['a'..'g']
        lenSamp   = (length . head) samples

        -- Update the possible sets based on the normal segments that can be on and off.

        updateSets :: [Char] -> [Char] -> P08ValidOptions
        updateSets normOn normOff = A.array ('a', 'g') (newSampOn ++ newSampOff)
          where
            newSampOn  = map (removeFrom normOff) sampleOn
            newSampOff = map (removeFrom normOn) sampleOff

        -- Remove the first set of chars from the second. Both must be sorted, and no duplicates.

        removeThese :: String -> String -> String
        removeThese [] ys = ys
        removeThese _ [] = []
        removeThese xss@(x : xs) yss@(y : ys)
          | x < y = removeThese xs yss
          | x == y = removeThese xs ys
          | otherwise = y : removeThese xss ys

        removeFrom :: String -> Char -> (Char, P08PossSet)
        removeFrom toRemove sampChar = (sampChar, newSet)
          where
            newSet = foldl' (flip S.delete) currSet toRemove
            currSet = validOptArr A.! sampChar

    -- Return if this string corresponds to a unique digit. (2 -> 1, 3 -> 7, 4 -> 4, 7 -> 8)

    uniqueDigLength :: String -> Bool
    uniqueDigLength str = strLen >= 2 && strLen <= 4 || strLen == 7
      where
        strLen = length str

    -- For this map, the key is a set of segments, which corresponds to a particular digit. This is
    -- used to take a set of segments lit up and match it to a digit.

    segmentsToDigMap :: M.Map P08SegmentSet Int
    segmentsToDigMap = M.fromList [(S.fromList ['a','b','c','e','f','g'], 0),
                                   (S.fromList ['c','f'], 1),
                                   (S.fromList ['a','c','d','e','g'], 2),
                                   (S.fromList ['a','c','d','f','g'], 3),
                                   (S.fromList ['b','c','d','f'], 4),
                                   (S.fromList ['a','b','d','f','g'], 5),
                                   (S.fromList ['a','b','d','e','f','g'], 6),
                                   (S.fromList ['a','c','f'], 7),
                                   (S.fromList ['a','b','c','d','e','f','g'], 8),
                                   (S.fromList ['a','b','c','d','f','g'], 9)]

    -- Identify the characters that can make up a signal pattern, and only accept those.

    segmentIDChar :: Parser Char
    segmentIDChar = sat (\c -> c >= 'a' && c <= 'g')

    -- This defines a segment pattern with one or more characters and optional space at the front.
    -- Note that we return this sorted, which will make processing easier in part B.

    segmentID :: Parser String
    segmentID = do
      space
      segList <- some segmentIDChar
      return (sort segList)

    -- Parse the initial signal patterns, then the output values. Sort the first set by length,
    -- and each string will be sorted alphabetically.

    parseSignalPatterns :: Parser P08SignalsForOutput
    parseSignalPatterns = do
      firstCodes <- many segmentID
      space
      _ <- symbol "|"
      secondCodes <- many segmentID
      let firstCodesByLen = (map snd . sort . map (\xs -> (length xs, xs))) firstCodes
      return (firstCodesByLen, secondCodes)

--
-- Code for Puzzle 9.
--

type P09Coord = (Int, Int)
type P09HeightArray = UA.Array (Int, Int) Int
type P09CoordAndHeight = (P09Coord, Int)

puzzle_09 :: IO (Int, Int)
puzzle_09 = do
  heightArr <- readArrayOfDigits True "puzzle_09.inp" "9"

  let lowPoints  = (filter (isLowPoint heightArr) . UA.assocs) heightArr

      -- The answer to part 1.

      riskLevels = map ((+ 1) . snd) lowPoints

      -- Part 2 is a bit more complicated, but pretty straightforward.

      basins = map (findBasinCoords heightArr . fst) lowPoints
      productThreeLargest = (product . take 3 . sortBy (flip compare) . map length) basins

  return (sum riskLevels, productThreeLargest)

  where

    -- From a low point, systematically move out collecting the coordinates that have heights that
    -- cause them to drain into the low point and are not height 9. This will return a list of all
    -- of the coordinates in the basin. This could be written in a faster way by using a mutable
    -- array to keep track of visited coordinates while mapping out the basins, but as it is, it
    -- took less than a millisecond for the full puzzle.

    findBasinCoords :: P09HeightArray -> P09Coord -> [P09Coord]
    findBasinCoords arr coord = fbc [coord] S.empty
      where

        -- The recursive part of this process takes in a set of the coordinates known to be in the
        -- basin, and a list of coordinates that are in the basin, but before inserting them in the
        -- set, we need to look at the next door coordinates.

        fbc :: [P09Coord] -> S.Set P09Coord -> [P09Coord]
        fbc [] basinCoords  = S.toList basinCoords
        fbc (currCoord : remainingCoords) basinCoords
          | S.member currCoord basinCoords = fbc remainingCoords basinCoords
          | otherwise = fbc newPossCoords newBasinCoords
          where
            newBasinCoords = S.insert currCoord basinCoords
            newPossCoords  = validNextDoor ++ remainingCoords
            validNextDoor = (map fst . filter ((> height) . snd) . filter ((< 9) . snd)
                             . map (\cd -> (cd, arr UA.! cd))
                             . filter (not . flip S.member basinCoords)) nextDoor
            nextDoor = nextDoorCoords arr currCoord
            height = arr UA.! currCoord

    -- Return true if this point is lower than the adjacent array locations.

    isLowPoint :: P09HeightArray -> P09CoordAndHeight -> Bool
    isLowPoint arr (coord, height) = all ((> height) . (arr UA.!)) (nextDoorCoords arr coord)

    -- Return the list of coordinates next to (horizontally or vertically) the one passed in that
    -- are within the array bounds.

    nextDoorCoords :: P09HeightArray -> P09Coord -> [P09Coord]
    nextDoorCoords arr coord = filter (inBoundsXY (UA.bounds arr)) (nextDoorHV coord)

--
-- Code for Puzzle 10.
--

-- The kinds of chunks as defined by the problem. The TopSeq one is for the top-level.

data P10ChunkT = P10Curly | P10Parens | P10Angle | P10Square | P10TopSeq deriving Show

-- These hold the different types of chunks. The valid one is the primary one that is used and holds
-- the chunk type as well as the list of nested chunks. The others are for different error conditions.

data P10Chunk = P10Valid P10ChunkT [P10Chunk]
                | P10CloseChar Char [P10Chunk]
                | P10Incomplete String
                | P10Corrupt Char [P10Chunk]
                | P10BadChar Char
                | P10Nothing
                deriving Show

puzzle_10 :: IO (Int, Int)
puzzle_10 = do
  navLines <- fmap lines (readFile "puzzle_10.inp")

  let parseResults = map readChunksTop navLines
      chunkResults = map snd parseResults
      corruptScore = (sum . map corruptValue) chunkResults
      scoresMissing = (sort . map sumMissing . foldr missingIfIncomplete []) chunkResults
      scorePartB = scoresMissing !! (length scoresMissing `quot` 2)

  return (corruptScore, scorePartB)

  where

    -- This is called at the top level of the string. It converts a nothing, which is expected
    -- (since it will read to the end of the string), to a valid chunk with P10TopSeq as the chunk
    -- type. For an incomplete sequence, reverse the list of missing closing characters, and for
    -- other error types, just return them as they are. We return a pair from this function
    -- including the remaining string that didn't get parsed, but this is only in error cases, and
    -- isn't used by this problem.

    readChunksTop :: String -> (String, P10Chunk)
    readChunksTop str = case failChunk of
      P10Nothing              -> (remainStr, P10Valid P10TopSeq chunks)
      (P10Incomplete missing) -> (remainStr, P10Incomplete (reverse missing))
      _                       -> (remainStr, failChunk)
      where
        (remainStr, failChunk, chunks) = readChunks (str, P10Nothing, [])

    -- This function will read a series of, possibly nested, chunks from the string in the first
    -- triple. It will stop when it finds an error or runs out of characters. The second item in the
    -- triple will be the final thing found. The third item will be the list of correctly parsed
    -- chunks before the last one. Note that more could be done with the error cases if there was a
    -- description of what should happen when they are found.

    readChunks :: (String, P10Chunk, [P10Chunk]) -> (String, P10Chunk, [P10Chunk])
    readChunks ([], _, accChunks) = ([], P10Nothing, reverse accChunks)
    readChunks (c : cs, _, accChunks)
      | c == '(' = readToClosingCharAndCont failChunk ')' P10Parens
      | c == '[' = readToClosingCharAndCont failChunk ']' P10Square
      | c == '{' = readToClosingCharAndCont failChunk '}' P10Curly
      | c == '<' = readToClosingCharAndCont failChunk '>' P10Angle
      | c == ')' || c == ']' || c == '}' || c == '>' = corruptResult
      | otherwise = (cs, P10BadChar c, reverse accChunks)
      where
        (newStr, failChunk, _) = readChunks (cs, P10Nothing, [])
        corruptResult = (cs, P10CloseChar c (reverse accChunks), [])

        -- Given the failing chunk or closing character from the current read do what is needed to
        -- pass the information up to the current level.

        readToClosingCharAndCont :: P10Chunk -> Char -> P10ChunkT -> (String, P10Chunk, [P10Chunk])
        readToClosingCharAndCont (P10Valid _ _) _ _ = error "Shouldn't find a valid here."
        readToClosingCharAndCont (P10CloseChar ch chks) expectedCloseCh chunkType

          -- We found a valid chunk with matching begin and end chars, and possibly sub-chunks. Add
          -- this valid chunk to the current list, and keep going.

          | ch == expectedCloseCh
            = readChunks (newStr, P10Nothing, P10Valid chunkType (reverse chks) : accChunks)

          -- We found a closing character that doesn't match the beginning character, so this is a
          -- corrupt string. Return the mis-matched closing character as well as any valid chunks
          -- read before it.

          | otherwise = (newStr, P10Corrupt ch (reverse chks), reverse accChunks)

        -- For an incomplete chunk, add the current expected character to the front of the missing
        -- list, and pass back.

        readToClosingCharAndCont (P10Incomplete missing) expectedCloseCh _
          = (newStr, P10Incomplete (expectedCloseCh : missing), [])

        -- If we found a corrupted chunk, just pass this information back up.

        readToClosingCharAndCont (P10Corrupt _ _) _ _ = (newStr, failChunk, [])

        -- If we found a character that is not any kind of opening or closing character, pass it
        -- back up the chain.

        readToClosingCharAndCont (P10BadChar _) _ _ = (newStr, failChunk, [])

        -- If we found a nothing, that means the string ended prematurely, and we want to pass back
        -- an incomplete indication with the expected character in a list to build on as we move up
        -- through other expected characters

        readToClosingCharAndCont P10Nothing expectedCloseCh _
          = (newStr, P10Incomplete [expectedCloseCh], [])

    -- Return the illegal closing character value for a corrupt entry and 0 for the rest.

    corruptValue :: P10Chunk -> Int
    corruptValue (P10Corrupt ch _) = valIllegalChar ch
    corruptValue _ = 0

    -- Return the missing list of characters for an incomplete entry and nothing otherwise.

    missingIfIncomplete :: P10Chunk -> [String] -> [String]
    missingIfIncomplete (P10Incomplete str) acc = str : acc
    missingIfIncomplete _ acc = acc

    -- Sum the values of the missing closing characters according to the problem.

    sumMissing :: String -> Int
    sumMissing = foldl' (\acc c -> acc * 5 + valMissingChar c) 0

    -- The value of an unexpected end chunk character.

    valIllegalChar :: Char -> Int
    valIllegalChar ch
      | ch == ')' = 3
      | ch == ']' = 57
      | ch == '}' = 1197
      | ch == '>' = 25137
      | otherwise = 0

    -- The value of an unexpected end chunk character.

    valMissingChar :: Char -> Int
    valMissingChar ch
      | ch == ')' = 1
      | ch == ']' = 2
      | ch == '}' = 3
      | ch == '>' = 4
      | otherwise = 0

--
-- Code for Puzzle 11.
--

type P11Coord      = (Int, Int)
type P11EnergyArr  = UA.Array P11Coord Int
type P11FlashedArr = UA.Array P11Coord Bool

puzzle_11 :: IO (Int, Int)
puzzle_11 = do
  initialEnergyArr <- readArrayOfDigits True "puzzle_11.inp" "11"

  let cycles = 100
      octopusCount = (length . UA.elems) initialEnergyArr
      statesAfterSteps = iterate nextStep (0, 0, initialEnergyArr)
      (_, totalFlashes, _) = statesAfterSteps !! cycles
      countToSimulFlash = (length . takeWhile (\(c, _, _) -> c /= octopusCount)) statesAfterSteps
  
  return (totalFlashes, countToSimulFlash)

  where

    -- Given an energy array and a total flash count, make one step, and return the new energy
    -- array, the count of flashes from this step, and the total flash count.

    nextStep :: (Int, Int, P11EnergyArr) -> (Int, Int, P11EnergyArr)
    nextStep (_, currFlashC, currEnergyArr) = allFlashC `seq` (nowFlashC, allFlashC, newEnergyArr)
      where
        allFlashC = currFlashC + nowFlashC

        -- For this step, iterate until no more flashes occur. Return the resulting energy array as
        -- well as an array indicating the elements that flashed.

        (updatedFlashedArr, newEnergyArr) = iterateWhileChanges (emptyFlashedArr, energyInc1Arr)

        -- Count the number of elements that were flashed in this iteration.

        nowFlashC = (length . filter id . UA.elems) updatedFlashedArr

        -- Increment the energy counts by 1 for all elements of the energy array for the start of a
        -- step. Also, create a corresponding array indicating the elements that have flashed this
        -- step. These all start out as false before we iterate.

        energyInc1Arr   = fmap (+ 1) currEnergyArr
        emptyFlashedArr = fmap (const False) currEnergyArr

        -- Take in an array indicating the elements that have already flashed on this step and an
        -- array of the current energy values. Find any energy values that are ready to flash, flash
        -- them noting the coordinates around them to increment, and record the flashed elements and
        -- update the energy array for the next iteration. If there are no elements ready to flash,
        -- then we are done with this step, and return the array indicating elements flashed and the
        -- new energy values. The energy array returned from here or passed to the next iteration
        -- will have all of the flashed elements from this step so far set to 0. Note that this
        -- could have been done with the iterate function, but identifying when we are done would
        -- have been more difficult.

        iterateWhileChanges :: (P11FlashedArr, P11EnergyArr) -> (P11FlashedArr, P11EnergyArr)
        iterateWhileChanges (flashed, energy)

          -- If there were no flashes on this iteration, then return the current arrays.

          | null flashedInds = (flashed, energy)

          -- Call this function again with the updated arrays.

          | otherwise = iterateWhileChanges (nextFlashedArr, nextEnergyArr)
          where

            -- The array indicating the elements that have flashed in this or prior iterations.

            nextFlashedArr = flashed UA.// zip flashedInds (repeat True)

            -- The energy array after this iteration, with all elements set to 0 that have flashed
            -- in this or prior iterations.

            nextEnergyArr  = newEnergyFlashedNotZeroed UA.// zip allFlashedInds (repeat 0)

            -- Here is a lit of all of the indices that have already flashed in this step.

            allFlashedInds = (map fst . filter snd . UA.assocs) nextFlashedArr

            -- Update the energy array from flash increments. This will have elements that have
            -- flashed this time or in prior iterations that are not zero due to these incrments.

            newEnergyFlashedNotZeroed  = UA.accum (+) energy (zip incInds (repeat 1))

            -- Get the coordinates that are ready to flash and the coordinates that will be
            -- incremented due to those flashes.

            (flashedInds, incInds) = foldl' flashAcc ([], []) (UA.assocs energy)

        -- If the value of the given coordinate is over 9, then add it to the list of flashed
        -- coordinates, and add all of the coordinates around it to the list of coordinates to
        -- increment.

        flashAcc :: ([P11Coord], [P11Coord]) -> (P11Coord, Int) -> ([P11Coord], [P11Coord])
        flashAcc acc@(accFInds, accIInds) (ind, val)
          | val > 9 = (ind : accFInds, surroundingInds ++ accIInds)
          | otherwise = acc
          where
            surroundingInds = nextDoorCoords ind

        -- Return the list of coordinates next to (horizontally, vertically, and diagonally) the one
        -- passed in that are within the array bounds. Note that when generating the list of next
        -- door coordinates, we drop the first one, which will be the coordinate passed in.

        nextDoorCoords :: P11Coord -> [P11Coord]
        nextDoorCoords coord = filter (inBoundsXY (UA.bounds currEnergyArr)) (nextDoorHVD coord)

--
-- Code for Puzzle 12.
--

type P12GraphMap = M.Map String [String]
type P12GraphMapWithSize = M.Map String (Bool, [String])

-- hlint generates an Eta reduce suggestion for the dfsPaths2 function, which I want to leave as it
-- is for clarity. This will stop all such warnings for the function puzzle_12 and those functions
-- inside.

{-# ANN puzzle_12 "HLint: ignore Eta reduce" #-}

puzzle_12 :: IO (Int, Int)
puzzle_12 = do
  parseRes <- fmap (parse parseInput) (readFile "puzzle_12.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 12.")

  let graphMap   = (fst . head) parseRes
      pathsToEnd = dfsPaths2 graphMap False startCaveName
      pathsToEndOneSmallRepeat = dfsPaths2 graphMap True startCaveName
      pathCount  = length pathsToEnd
      pathCountSmallRepeat = length pathsToEndOneSmallRepeat

  return (pathCount, pathCountSmallRepeat)

  where

    startCaveName = "start"
    endCaveName   = "end"

    -- Do a depth-first search from the start and return all of the distinct paths to the end. In
    -- this problem, we really just need the count of paths, but even though it's slower to
    -- accumulate the path information, it would normally be useful to have.

    dfsPaths2 :: P12GraphMapWithSize -> Bool -> String -> [[String]]
    dfsPaths2 caveMap canVisitSmallTwice startCave = dfs canVisitSmallTwice [] S.empty [] startCave
      where
        dfs :: Bool -> [[String]] -> S.Set String -> [String] -> String -> [[String]]
        dfs canVisitSmallAgain accPaths currPathCaves currPathList currCave

          -- If the current cave name isn't in the map, then we have a problem.

          | isNothing entryM = error ("Undefined cave name: " ++ currCave)

          -- This is the case where we can't go into a cave because it's small and we've been there
          -- before, and we can't use our one opportunity to go into a small cave twice (part 2 of
          -- this problem).

          | isSmallCave && not canVisitSmallAgain && seenBeforeInPath = accPaths

          -- We have reached the end. Add this path to the list of full paths and return.

          | currCave == endCaveName = let currFullPath = endCaveName : currPathList
                                      in reverse currFullPath : accPaths

          -- We are fine going through the current cave, so explore all of the caves adjacent to it,
          -- and accumulate all of the full paths to the end that we find.

          | otherwise = foldl' goDeeper accPaths nextDoorCaves

          where

            seenBeforeInPath = S.member currCave currPathCaves

            -- Lookup the current cave in the map and find out if it is a small cave and get the
            -- list of adjacent caves.

            (isSmallCave, nextDoorCaves) = fromJust entryM
            entryM = M.lookup currCave caveMap

            -- Do a recursive call with updated parameters.

            goDeeper :: [[String]] -> String -> [[String]]
            goDeeper accum = dfs newCanVisitSmallAgain accum setWithCurr newPathList

            -- Updated values for the recursive call.

            setWithCurr = S.insert currCave currPathCaves
            newPathList = currCave : currPathList
            newCanVisitSmallAgain
              | canVisitSmallAgain && isSmallCave && seenBeforeInPath = False
              | otherwise = canVisitSmallAgain

    -- Parse the input for this problem, returning a map of nodes where each is associated with a
    -- list of nodes it is connected to by a single edge. Also, noted is whether this is a small
    -- cave.

    parseInput :: Parser P12GraphMapWithSize
    parseInput = do
      connectedNodes <- many parseNodePair
      let graphMap = foldl' addConnectedNodes M.empty connectedNodes
          graphMapWithSize = M.mapWithKey addSizeBool graphMap

          -- Add indication that this is a small cave, and remove the start cave as a destination
          -- because we never want to go there.

          addSizeBool k xs = (isLower (head k), filter (/= startCaveName) xs)

      return graphMapWithSize

      where

        -- Add an edge to the graph. Also, insure that both nodes are in the graph.

        addConnectedNodes :: P12GraphMap -> (String, String) -> P12GraphMap
        addConnectedNodes acc (str1, str2) = foldl' insertEdge acc [(str1, str2), (str2, str1)]
          where
            insertEdge accum (src, dest)
              | M.member src accum = M.insertWith (\[x] ac -> x : ac) src [dest] accum
              | otherwise = M.insert src [dest] accum

    -- Parse two strings separated by a hyphen.

    parseNodePair :: Parser (String, String)
    parseNodePair = do
      ident1 <- ident
      _ <- symbol "-"
      ident2 <- ident
      _ <- space
      return (ident1, ident2)

--
-- Code for Puzzle 13.
--

type P13PaperArr = UA.Array (Int, Int) Bool

puzzle_13 :: IO (Int, String)
puzzle_13 = do
  parseRes <- fmap (parse parseInput) (readFile "puzzle_13.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 13.")

  let (coords, folds) = (fst . head) parseRes

  -- Check to make sure there are both coordinates and folds.

  when (null coords || null folds)
       (ioError $ userError "Either empty coordinates or empty folds in puzzle 13.")

  let maxB = ((maximum . map fst) coords, (maximum . map snd) coords)
      emptyArr   = genEmptyArr ((0, 0), maxB)
      initialArr = emptyArr UA.// zip coords (repeat True)

      -- Generate the first answer.

      firstFoldArr = foldPaper initialArr (head folds)
      countAfterFirst = (length . filter id . UA.elems) firstFoldArr

      -- Generate the second answer. This answer actually comes from printing out a grid and
      -- identifying letters, which I did using the mapM_ below, but to streamline the output, I use
      -- the count computed here to insure the calculation is done before returning the answer I
      -- read off the grid.

      afterAllFolds = foldl' foldPaper initialArr folds
      ((_, _lowY), (_, _highY)) = UA.bounds afterAllFolds
      countAfterAll = (length . filter id . UA.elems) afterAllFolds

  -- Uncomment to print out the result array to see the 8 letters.
  -- mapM_ (_printRow afterAllFolds) [_lowY.._highY]

  return (countAfterFirst, countAfterAll `seq` "AHPRPAUZ")

  where

    -- Print a row of the array using '.' and '#'. Define with an '_' at the start of the name to
    -- avoid a compiler warning that it isn't used.

    _printRow :: P13PaperArr -> Int -> IO ()
    _printRow arr yVal = do
      let ((lowX, _), (highX, _)) = UA.bounds arr
          lineStr = map (\x -> if arr UA.! (x, yVal) then '#' else '.') [lowX..highX]
      putStrLn $ "  " ++ lineStr

    -- Return the array after folding it on the given x or y line.

    foldPaper :: P13PaperArr -> (Bool, Int) -> P13PaperArr
    foldPaper currArr (alongX, line) = newArr
      where
        newArr   = emptyArr UA.// foldr addWithFoldIfOn [] (UA.assocs currArr)
        emptyArr = genEmptyArr newBounds
        newBounds
          | alongX = (mins, (line - 1, oldMaxY))
          | otherwise = (mins, (oldMaxX, line - 1))
        (mins, (oldMaxX, oldMaxY)) = UA.bounds currArr

        -- Used to fold through the old array, converting the 'on' elements to their new coordinates
        -- in the folded array.

        addWithFoldIfOn :: ((Int, Int), Bool) -> [((Int, Int), Bool)] -> [((Int, Int), Bool)]
        addWithFoldIfOn (coord@(x, y), switchedOn) acc

          -- If the dot isn't on at this location, there's nothing to do.

          | not switchedOn = acc

          -- If there is a dot on the fold line, which there shouldn't be, then ignore it.

          | alongX && x == line = acc
          | not alongX && y == line = acc

          -- Add that this coordinate is set to the accumulating list.

          | otherwise = (newCoord, True) : acc
          where
            foldedVal w = line - (w - line)
            newCoord
              | alongX = if x < line then coord
                         else (foldedVal x, y)
              | otherwise = if y < line then coord
                            else (x, foldedVal y)

    -- Return an array filled with False with the given bounds.

    genEmptyArr :: ((Int, Int), (Int, Int)) -> P13PaperArr
    genEmptyArr bounds@((minX, minY), (maxX, maxY))
      = UA.array bounds [((x, y), False) | x <- [minX..maxX], y <- [minY..maxY]]

    -- Parse the input for this puzzle.

    parseInput :: Parser ([(Int, Int)], [(Bool, Int)])
    parseInput = do
      coords <- many parseCoordPair
      folds  <- many parseFold
      return (coords, folds)

    -- Parse a coordinate pair and and space after it, returning the pair.

    parseCoordPair :: Parser (Int, Int)
    parseCoordPair = do
      x <- int
      _ <- symbol ","
      y <- int
      _ <- space
      return (x, y)

    -- Parse one of the fold lines at the end, returning a pair with True for an X-fold and False
    -- for a Y-fold. Also return the index of the fold. Parse any additional space at the end.

    parseFold :: Parser (Bool, Int)
    parseFold = do
      _ <- symbol "fold along "
      ch <- letter
      _ <- symbol "="
      val <- int
      _ <- space
      return (ch == 'x', val)

--
-- Code for Puzzle 14.
--

type P14InsertMap    = M.Map String Char
type P14CharCounts   = M.Map Char Integer
type P14StrOccCounts = M.Map (String, Int) P14CharCounts

-- The idea for solving this is to approach the string and the number of each type of character in
-- the expanded version, as a recursive search with a map keyed off of character pairs and
-- depth. The substitution template that is passed and is used at each level, and as we descend in
-- depth, we populate the map and then as we encounter pairs and depths that we have used before, we
-- just incorporate those counts associated with each character. For example, if we are at the sixth
-- level down and wanting to go to the 10th, the map could contain a pair with the letters "NC" at
-- the depth of six. The first time we run into this, we compute the answer recursively (although
-- within that recursion may use cached data), but then when we run across another "NC" on the same
-- level, we find it in the map and take the union of the character counts with the ones so far to
-- combine it with.
-- With the substitutions as they are in this problem, there are a lot of common pairs as we grow
-- the string, and we just cache the results for each pair and level when we first encounter it, and
-- then used the cached counts all of the other times.

puzzle_14 :: IO (Integer, Integer)
puzzle_14 = do
  parseRes <- fmap (parse parseInput) (readFile "puzzle_14.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 14.")

  let (initialTemplate, pairInsertionRules) = (fst . head) parseRes
      insertionPairMap = M.fromList pairInsertionRules

      resultA = getResultForDepth insertionPairMap initialTemplate 10
      resultB = getResultForDepth insertionPairMap initialTemplate 40

  return (resultA, resultB)

  where

    -- Get the result for the given depth.

    getResultForDepth :: P14InsertMap -> String -> Int -> Integer
    getResultForDepth pairMap initialString depth = maximum resultElems - minimum resultElems
      where
        countsMap = snd $ countOccurrances pairMap M.empty depth initialString
        resultElems = M.elems countsMap

    -- Recursively accumulate the number of counts of each character at the given level.

    countOccurrances :: P14InsertMap -> P14StrOccCounts -> Int -> String
                        -> (P14StrOccCounts, P14CharCounts)
    countOccurrances _ currMap _ [] = (currMap, M.empty)
    countOccurrances insertMap currMap deeperToGo currStr = result
      where

        -- Walk through all of the pairs of adjacent characters for this string.

        result = foldl' processPairsIntoMap (currMap, M.singleton (head currStr) 1) pairedChars
        pairedChars = zip currStr (tail currStr)

        -- Handle a pair of adjacent characters.

        processPairsIntoMap :: (P14StrOccCounts, P14CharCounts) -> (Char, Char)
                               -> (P14StrOccCounts, P14CharCounts)
        processPairsIntoMap (mapIn, charMapIn) (ch1, ch2)

          -- If we find the pair of characters and the current depth in the cache of counts, just
          -- include it in the accumulated counts and move on.

          | isJust foundMapM = let newCharMap = addInCurrResultAndSubOutFirstChar foundMap
                               in  (mapIn, newCharMap)

          -- If we are at the bottom level, create a record in the map and merge the counts in with
          -- the accumulated counts.

          | deeperToGo == 0  = let mapOut = M.insert (keyForPair, deeperToGo) pairMap mapIn
                                   newCharMap = addInCurrResultAndSubOutFirstChar pairMap
                               in  (mapOut, newCharMap)

          -- Here we have a character pair and depth that we haven't seen before. We insert a
          -- character between the two based on the insertion map, then do a recursive call with the
          -- depth one less. When that completes, put the result into the map (that came back from
          -- the recursive call) so we don't have to do it again.

          | otherwise = let newStr = insertPairs insertMap keyForPair
                            (mapOutBelow, charMapBelow) = countOccurrances insertMap mapIn (deeperToGo - 1) newStr
                            charMapOut = addInCurrResultAndSubOutFirstChar charMapBelow
                            mapOut = M.insert (keyForPair, deeperToGo) charMapBelow mapOutBelow
                        in (mapOut, charMapOut)
          where

            -- The cached map result (if it exists) for the current pair of characters.

            foundMap  = fromJust foundMapM
            foundMapM = M.lookup (keyForPair, deeperToGo) mapIn
            keyForPair = [ch1, ch2]
            pairMap = if ch1 == ch2 then M.singleton ch1 2 else M.fromList [(ch1, 1), (ch2, 1)]

            -- For the results of a particular pair of characters, add the counts in with the
            -- current set of counts being accumulated at this level. Note that we subtract out the
            -- first character in the pair, or we would double count it as first and second.

            addInCurrResultAndSubOutFirstChar :: P14CharCounts -> P14CharCounts
            addInCurrResultAndSubOutFirstChar charMapToAdd = newCharMap
              where
                combinedCharMap = M.unionWith (+) charMapToAdd charMapIn
                newCharMap = M.adjust (\x -> x - 1) ch1 combinedCharMap

    -- Perform a single substitution step on the input string, using the rules in the insert map.

    insertPairs :: P14InsertMap -> String -> String
    insertPairs _ [] = []
    insertPairs _ [x] = [x]
    insertPairs pairRules (ch1 : ch2 : chs)
      | isNothing foundM = ch1 : continuation
      | otherwise = ch1 : fromJust foundM : continuation
      where
        foundM = M.lookup [ch1, ch2] pairRules
        continuation = insertPairs pairRules (ch2 : chs)

    -- Parse the entire input.

    parseInput :: Parser (String, [(String, Char)])
    parseInput = do
      polyTemplate <- ident
      _ <- space
      pairInsertRules <- many pairInsertRule
      return (polyTemplate, pairInsertRules)

    -- Parse a single substitution template.

    pairInsertRule :: Parser (String, Char)
    pairInsertRule = do
      poly <- ident
      _ <- space
      _ <- symbol "->"
      _ <- space
      insrt <- ident
      _ <- space
      return (poly, head insrt)

--
-- Code for Puzzle 15.
--

-- Use the fgl graph library to encode the array, then use the shortest path algorithm.

type P15ArrCoord  = (Int, Int)
type P15NodeLabel = P15ArrCoord
type P15Distance  = Int
type P15RiskArray = UA.Array P15ArrCoord Int
type P15RiskGraph = GRPT.Gr P15NodeLabel P15Distance

puzzle_15 :: IO (Int, Int)
puzzle_15 = do
  riskArrA <- readArrayOfDigits False "puzzle_15.inp" "15"

  let pathRiskA = computeMinPath riskArrA

      riskArrB  = genTiledArrayFromInitTile riskArrA 5 5
      pathRiskB = computeMinPath riskArrB

  return (pathRiskA, pathRiskB)

  where

    -- Compute the risk of the best path from the start to the end of the array.

    computeMinPath :: P15RiskArray -> Int
    computeMinPath riskArr = pathRiskSum
      where
        pathRiskSum = (sum . map snd . drop 1) pathCoords
        pathCoords = map (nodeNumToCoord riskArr riskGraph) path
        path = fromJust (GRSP.sp startNode destNode riskGraph)
        (startNode, destNode, riskGraph) = genGraphFromArray riskArr

    -- Given a nodeID from the given graph, return the node label, which in this case is the
    -- corresponding array coordinate, along with the contents of this array element.

    nodeNumToCoord :: P15RiskArray -> P15RiskGraph -> Int -> (P15ArrCoord, Int)
    nodeNumToCoord arr graph nodeInd
      | isNothing nodeLabelM = error "Bad node ID in nodeNumToCoord."
      | otherwise = (coord, arr UA.! coord)
      where
        coord      = fromJust nodeLabelM
        nodeLabelM = GR.lab graph nodeInd

    -- Generate a multiple tile array from the original corner tile using the adjustments defined in
    -- the problem for the values held.

    genTiledArrayFromInitTile :: P15RiskArray -> Int -> Int -> P15RiskArray
    genTiledArrayFromInitTile cornerTileArr xTiles yTiles = multiTileArray
      where
        multiTileArray = UA.array fullArrBounds fullInitList
        fullArrBounds = ((0, 0),
                         (xTiles * (highTX - lowTX + 1) - 1, yTiles * (highTY - lowTY + 1) - 1))
        fullInitList = concatMap (genTileInitVals cornerTileArr)
                                 [(x, y) | x <- [0..(xTiles - 1)], y <- [0..(yTiles - 1)]]
        ((lowTX, lowTY), (highTX, highTY)) = UA.bounds cornerTileArr
        
        -- Given a tile x and y location, generate the initialization list for that tile.

        genTileInitVals :: P15RiskArray -> (Int, Int) -> [(P15ArrCoord, Int)]
        genTileInitVals arr (tileX, tileY) = map updateForThisTile (UA.assocs arr)
          where
            updateForThisTile :: (P15ArrCoord, Int) -> (P15ArrCoord, Int)
            updateForThisTile ((x, y), val) = newX `seq` ((newX, newY), newVal)
              where
                newX = newY `seq` tileX * (highTX - lowTX + 1) + lowTX + x
                newY = newVal `seq` tileY * (highTY - lowTY + 1) + lowTY + y
                newVal = (head . dropWhile (> 9) . iterate (subtract 9)) (val + toAddToValue)
            toAddToValue = tileX + tileY
        
    -- Generate a graph representing the risk grid from the given array.

    genGraphFromArray :: P15RiskArray -> (Int, Int, P15RiskGraph)
    genGraphFromArray arr = (startNode, destNode, GR.mkGraph nodeList edgeList)
      where

        -- I flipped the rows when they were read in so that the path goes from (0, 0) to the far
        -- opposite corner.

        startNode = correspondingNode (0, 0)
        destNode  = correspondingNode (highX, highY)

        -- The nodes are labeled from 0, and the associated array index is associated as a label.
        -- There is an edge created from each element of the array to all adjacent cells (not
        -- including diagonal). The cost of each of these edges is the risk associated with the
        -- destination cell.

        nodeList = zip [0..] arrIndices
        edgeList = concatMap genOutEdges arrIndices

        arrIndices = UA.indices arr
        arrBounds@((lowX, lowY), (highX, highY)) = UA.bounds arr

        -- Given the (x, y) indices of a cell in the array, determine the graph node index.

        correspondingNode :: (Int, Int) -> Int
        correspondingNode (x, y) = (x - lowX) * (highY - lowY + 1) + (y - lowY)

        -- Given a cell in the array, determine all of the graph edges associated with the cost of
        -- each edge being the value in the adjacent cell.

        genOutEdges :: (Int, Int) -> [(Int, Int, P15Distance)]
        genOutEdges srcCoord = map genEdgeInfo nextDoorCoords
          where
            srcNode = correspondingNode srcCoord
            nextDoorCoords = filter (inBoundsXY arrBounds) (nextDoorHV srcCoord)
            genEdgeInfo :: (Int, Int) -> (Int, Int, P15Distance)
            genEdgeInfo dstCoord = (srcNode, dstNode, dist)
              where
                dstNode = correspondingNode dstCoord
                dist = arr UA.! dstCoord

--
-- Code for Puzzle 16.
--

-- Define a class for summing the version numbers of a nested packet structure. Also one for the
-- value computed from this packet down.  Instances of these function are created for P16Packet and
-- P16Code

class P16VersionSum a where
  versionSum :: a -> Int

class P16Value a where
  valueComputation :: a -> Int64

-- This hold a packet with it's version number, its typeID, and a P16Code object.  Define versionSum
-- to add the version number the to recursively defined versionSum of the associated P16Code.

data P16Packet = P16Packet Int P16Code deriving Show
instance P16VersionSum P16Packet where
  versionSum (P16Packet version code) = version + versionSum code
instance P16Value P16Packet where
  valueComputation (P16Packet _ code) = valueComputation code

-- This holds a code object, which can be either a literal or an operator, which contains a list of
-- sub-packets. Note that an operator holds a type, which is used to differentiate between kinds of
-- operators. Define an instance of versionSum that will recursively sum the version numbers of any
-- operator sub packets. Similar for valueComputation. Note that in a more beefed up version of
-- this, it would likely have been better to define a separate P16Code for each individual
-- operation.

data P16Code = P16Literal Int64 | P16Operator Int [P16Packet] deriving Show
instance P16VersionSum P16Code where
  versionSum (P16Literal _) = 0
  versionSum (P16Operator _ packets) = (sum . map versionSum) packets
instance P16Value P16Code where
  valueComputation (P16Literal value) = value
  valueComputation (P16Operator typeCode packets)
    | typeCode == 0 = sum     valuesFromPackets
    | typeCode == 1 = product valuesFromPackets
    | typeCode == 2 = minimum valuesFromPackets
    | typeCode == 3 = maximum valuesFromPackets
    | typeCode == 5 = if firstVal  > secondVal then 1 else 0
    | typeCode == 6 = if firstVal  < secondVal then 1 else 0
    | typeCode == 7 = if firstVal == secondVal then 1 else 0
    | otherwise = error ("Undefine opcode " ++ show typeCode ++ " in valueComputation.")
    where
      [firstVal, secondVal]
        | null packets = error ("Null packets in valueComputation for typeCode " ++ show typeCode)
        | null (tail packets)
          = error ("A single packet in valueComputation for typeCode " ++ show typeCode)
        | (not . null . drop 2) packets
          = error ("More than two packets in valueComputation for typeCode " ++ show typeCode)
        | otherwise = valuesFromPackets
      valuesFromPackets = map valueComputation packets

puzzle_16 :: IO (Int, Int64)
puzzle_16 = do
  bitStr <- fmap (concatMap hexToBits) (readFile "puzzle_16.inp")

  -- Parse the binary recursive packet. Ignore the zeros that remain after the packet.

  let (packet, _) = getPacket bitStr
      verSum = versionSum packet
      value = valueComputation packet

  return (verSum, value)

  where

    -- Read a packet.

    getPacket :: [Int] -> (P16Packet, [Int])
    getPacket bitStr
      | typeCode == 4 = let (literal, remainingBitStrLit) = getLiteral afterType
                        in  (P16Packet version literal, remainingBitStrLit)
      | otherwise = let (operator, remainingBitStrOp) = getOperator typeCode afterType
                    in  (P16Packet version operator, remainingBitStrOp)
      where
        (typeCode, afterType) = getType afterVersion
        (version, afterVersion) = getVersion bitStr

    -- Return the version that is expect at the front of the bit string, and the remainder.

    getVersion :: [Int] -> (Int, [Int])
    getVersion bv = (bitListToInt versionBits, remainingBits)
      where
        (versionBits, remainingBits) = readNBits 3 bv

    -- Return the type that is expect at the front of the bit string, and the remainder.

    getType :: [Int] -> (Int, [Int])
    getType bv = (bitListToInt typeBits, remainingBits)
      where
        (typeBits, remainingBits) = readNBits 3 bv

    -- Return the literal that is expect at the front of the bit string, and the remainder. Note
    -- that the literal is read in a number of 4-bit chunks where a prefix bit indicates whether it
    -- is the last four-bit chunk.

    getLiteral :: [Int] -> (P16Code, [Int])
    getLiteral bitVec = (P16Literal $ fromIntegral (bitListToInt literalBits), remainingBits)
      where
        (literalBits, remainingBits) = readLiteralToEnd bitVec
        readLiteralToEnd :: [Int] -> ([Int], [Int])
        readLiteralToEnd bv
          | fBit == 0 = (lBits, xs)
          | otherwise = (lBits ++ remainLBits, remainBV)
          where
            (fBit : lBits, xs) = readNBits 5 bv
            (remainLBits, remainBV) = readLiteralToEnd xs

    -- Read an operator. We either read packets based on the number of bits or the number of
    -- packets, determined by the first bit we see.

    getOperator :: Int -> [Int] -> (P16Code, [Int])
    getOperator _ [] = error "Empty bit string for getOperator in Puzzle 16."
    getOperator typeCode (lenTypeID : bv)

      -- Decode the number of bits (stored in the first 15 bits of the stream), then take this
      -- number of bits, and read the packets from this part of the bitstream, then return the
      -- operator with these packets and the remaining bit stream after all these were read.

      | packetBitCount = let (subPacketCountBits, nextBits) = splitAt 15 bv
                             subPacketBitCount = bitListToInt subPacketCountBits
                             (subPacketBits, remainingBits) = splitAt subPacketBitCount nextBits
                             (subPackets, _) = getPackets maxBound subPacketBits
                         in  (P16Operator typeCode subPackets, remainingBits)

      -- Here, we read the number of sub-packets from the first 11 bits in the bit stream, then read
      -- that many after that to return as this packet.

      | otherwise = let (subPacketCountBits, nextBits) = splitAt 11 bv
                        subPacketCount = bitListToInt subPacketCountBits
                        (subPackets, followingBits) = getPackets subPacketCount nextBits
                    in  (P16Operator typeCode subPackets, followingBits)
      where

        -- Get packets until we either run out of bits or until we have read the given number of
        -- packets (first parameter). This function is called recursively.

        getPackets :: Int -> [Int] -> ([P16Packet], [Int])
        getPackets currLeft bitsLeft
          | currLeft == 0 || null bitsLeft = ([], bitsLeft)
          | otherwise = let (currPacket, newBitsLeft) = getPacket bitsLeft
                            (packetsLater, finalBitsLeft) = getPackets (currLeft - 1) newBitsLeft
                        in (currPacket : packetsLater, finalBitsLeft)

        -- Whether we read all of the packets in a bit stream of a given length or a certain number
        -- of packets.

        packetBitCount = lenTypeID == 0

    -- Convert a hex digit to a list of 4 0 or 1 integers.

    hexToBits :: Char -> [Int]
    hexToBits ch
      | (not . isHexDigit) ch = error "Non-hex digit in hexToBits from puzzle 16."
      | otherwise = (intToBinList 4 . digitToInt) ch

    -- Convert an int to a list of len 0s and 1s of the given length. Note that if there are more 1
    -- bits, they will be ignored..

    intToBinList :: Int -> Int -> [Int]
    intToBinList len val
      | len < 0 = error "Length less than zero in intToBinList from puzzle 16."
      | otherwise = go len val []
      where
        go 0 _ acc = acc
        go n x acc = let frontBit = x .&. 0x1
                         newAcc = frontBit `seq` frontBit : acc
                         newX = newAcc `seq` shiftR x 1
                     in  newX `seq` go (n - 1) newX newAcc

    -- Convert a list of binary digits (as ints) to an int.

    bitListToInt :: [Int] -> Int
    bitListToInt = go 0
      where
        go acc [] = acc
        go acc (x : xs) = let newAcc = 2 * acc + x
                          in  newAcc `seq` go newAcc xs

    -- Read the first n bits from the bit string and return them along with the remaing bits. If
    -- there aren't that many bits to read, generate an error.

    readNBits :: Int -> [Int] -> ([Int], [Int])
    readNBits len bitStr
      | length bitsTaken < len = error "Bit string not long enough in readNBits from puzzle 16."
      | otherwise = retVal
      where
        retVal@(bitsTaken, _) = splitAt len bitStr

--
-- Code for Puzzle 17.
--

-- Try firing over all possible (x, y) pairs up to a particular value. More could be done to narrow
-- the search, and we can take advantage of the fact that the Y location value will hit 0 again as
-- long as it starts positive. It is quite fast as it is.

type P17Location = (Int, Int)
type P17Velocity = (Int, Int)
type P17TargetRange = ((Int, Int), (Int, Int))
data P17HitOrMiss = Hit | MissHoriz | MissVert deriving (Eq, Show)

puzzle_17 :: IO (Int, Int)
puzzle_17 = do
  parseRes <- fmap (parse parseTargetArea) (readFile "puzzle_17.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 17.")

  let targetRange = (fst . head) parseRes
      velocitiesToTry = [(x, y) | x <- [(-300)..300], y <- [(-300)..300]]
      goodVels  = reportGoodVelsAndEndLocs targetRange velocitiesToTry
      bestFoundFull
        | null goodVels = Nothing
        | otherwise = Just (maximumBy (compare `on` (snd . (\(f, _, _) -> f))) goodVels)

      -- This is a cool function for getting a value out of a Maybe or a default if Nothing.

      bestFound = maybe 0 (\ (_, _, y) -> y) bestFoundFull
      validStartVels = length goodVels

  return (bestFound, validStartVels)

  where

    -- Given a target range and a list of initial XY velocities, return the list of velocities that
    -- are in the target at some point, and the location first in the target.

    reportGoodVelsAndEndLocs :: P17TargetRange -> [P17Velocity] -> [(P17Velocity, P17Location, Int)]
    reportGoodVelsAndEndLocs _ [] = []
    reportGoodVelsAndEndLocs targetRange (vel : vels)
      | hitOrMiss == Hit = (vel, lastLoc, maxYReached) : resultsFromRestOfList
      | otherwise = resultsFromRestOfList
      where
        (hitOrMiss, lastLoc, maxYReached) = initialVelHitsTarget targetRange vel
        resultsFromRestOfList = reportGoodVelsAndEndLocs targetRange vels

    -- Returns Hit and the location if this starting velocity results in the probe ending up in the
    -- terget range. If not, either return MissHoriz if it went past the target in the X-direction
    -- and MissVert if it went below the target with a zero or negative vertical velocity.

    initialVelHitsTarget :: P17TargetRange -> P17Velocity -> (P17HitOrMiss, P17Location, Int)
    initialVelHitsTarget targRange vel
      = srchForDone $ iterate stepVelocityAndPosition (vel, (0, 0), 0)
      where
        srchForDone :: [(P17Velocity, P17Location, Int)] -> (P17HitOrMiss, P17Location, Int)
        srchForDone [] = error "Empty list in srchForDone (puzzle_17)."
        srchForDone (((velX, velY), loc@(_, yLoc), maxYLoc) : locs)

          -- We have missed the target vertically if we are moving down and the location is below
          -- the target. If the location is below the target, but the vertical velocity is still
          -- positive, then we could still end up in the target.
 
          | velY <= 0 && locLTTargetFloor targRange loc = (MissVert, loc, newMaxYLoc)

          -- If we are either traveling in a positive X direction or stopped in the X direction and
          -- our location is right of the target, then we have missed it.

          | velX >= 0 && locGTTargetRight targRange loc = (MissHoriz, loc, newMaxYLoc)

          -- If we are either traveling in a negative X direction or stopped in the X direction and
          -- our location is left of the target, then we have missed it.

          | velX <= 0 && locLTTargetLeft targRange loc  = (MissHoriz, loc, newMaxYLoc)

          -- Return a hit if we are within the target.

          | locationInTarget targRange loc = (Hit, loc, newMaxYLoc)

          -- If none of the conditions above are true, then continue on with the list of locations.
 
          | otherwise = srchForDone locs
          where
            newMaxYLoc = if yLoc > maxYLoc then yLoc else maxYLoc

    -- Given a velocity and position, update the position with the given velocity, and then
    -- update the velocity for drag and gravity.

    stepVelocityAndPosition :: (P17Velocity, P17Location, Int) -> (P17Velocity, P17Location, Int)
    stepVelocityAndPosition ((xVel, yVel), (x, y), maxYLoc)
      = newX `seq` newY `seq` ((newXVel, newYVel), (newX, newY), newMaxYLoc)
      where
        (newXVel, newX) = stepXVelocityAndPosition (xVel, x)
        (newYVel, newY) = stepYVelocityAndPosition (yVel, y)
        newMaxYLoc = if newY > maxYLoc then newY else maxYLoc

    -- Step an x velocity and position.

    stepXVelocityAndPosition :: (Int, Int) -> (Int, Int)
    stepXVelocityAndPosition (xVel, x) = newX `seq` (newXVel, newX)
      where
        newX = newXVel `seq` x + xVel
        newXVel
          | xVel > 0 = xVel - 1
          | xVel < 0 = xVel + 1
          | otherwise = 0

    -- Step a y velocity and position.

    stepYVelocityAndPosition :: (Int, Int) -> (Int, Int)
    stepYVelocityAndPosition (yVel, y) = newY `seq` (newYVel, newY)
      where
        newY = newYVel `seq` y + yVel
        newYVel = yVel - 1

    -- Returns true of the given location is in the specified target range.

    locationInTarget :: P17TargetRange -> P17Location -> Bool
    locationInTarget ((xMin, xMax), (yMin, yMax)) (x, y)
      | x < xMin || x > xMax || y < yMin || y > yMax = False
      | otherwise = True

    -- Returns true if the given location is below the target.

    locLTTargetFloor :: P17TargetRange -> P17Location -> Bool
    locLTTargetFloor (_, (yMin, _)) (_, y) = y < yMin

    -- Returns true if the given location is to the right of the target.

    locGTTargetRight :: P17TargetRange -> P17Location -> Bool
    locGTTargetRight ((_, xMax), _) (x, _) = x > xMax

    -- Returns true if the given location is to the left of the target.

    locLTTargetLeft :: P17TargetRange -> P17Location -> Bool
    locLTTargetLeft ((xMin, _), _) (x, _) = x < xMin

    -- Parse the input: "target area: x=253..280, y=-73..-46".

    parseTargetArea :: Parser P17TargetRange
    parseTargetArea = do
      _ <- symbol "target area: x="
      xMin <- int
      _ <- symbol ".."
      xMax <- int
      _ <- symbol ", y="
      yMin <- int
      _ <- symbol ".."
      yMax <- int
      return ((xMin, xMax), (yMin, yMax))

--
-- Code for Puzzle 18.
--

-- Recursive definition of snailfish numbers. Technically, this allows a snailfish number to be a
-- single value, but for the purposes of this problem, this is simpler than defining a special
-- top-level type to preclude this possiblity.

data P18SFNumber = P18Value Int | P18Pair P18SFNumber P18SFNumber
instance Show P18SFNumber where
    show (P18Value val) = show val
    show (P18Pair leftSide rightSide) = mconcat ["[", show leftSide, ",", show rightSide, "]"]
data P18ReduceUp = P18ReduceUp { _changedBelow :: Bool
                               , _objectUp     :: P18SFNumber
                               , _distribLeft  :: Maybe Int
                               , _distribRight :: Maybe Int
                               }
data P18Direction = HugLeft | HugRight deriving (Eq, Show)
data P18Operation = Explode | Split deriving (Eq, Show)

puzzle_18 :: IO (Int, Int)
puzzle_18 = do
  parseRes <- fmap (map (parse parseSnailFishNumber) . lines) (readFile "puzzle_18.inp")

  when (badParseOfInputLines parseRes)
       (ioError $ userError "Parse error in input for puzzle 18.")

  let snailFishNumbers = map (fst . head) parseRes
      finalSum = sumList snailFishNumbers
      finalMagnitude = magnitude finalSum
      maxMagnitude = (maximum . map (magnitude . sumList) . concatMap (\xs -> [xs, reverse xs])
                      . subsetsOfSize 2) snailFishNumbers

  return (finalMagnitude, maxMagnitude)

  where

    -- Add up all of the numbes in the list in left to right order.

    sumList :: [P18SFNumber] -> P18SFNumber
    sumList = foldl1' addAndReduce

    -- Fully reduce the given SnailFish number.

    fullyReduce :: P18SFNumber -> P18SFNumber
    fullyReduce currSFNumber
      | reductionDone = fullyReduce nextSFNumber
      | otherwise = currSFNumber
      where
        (reductionDone, nextSFNumber) = reduceOneStep currSFNumber

    -- Compute the magnitude of the given SnailFish number.

    magnitude :: P18SFNumber -> Int
    magnitude (P18Value val) = val
    magnitude (P18Pair leftSide rightSide) = 3 * magnitude leftSide + 2 * magnitude rightSide

    -- First look for any pairs to explode. If none found, then look for numbers to split. Note that
    -- any exploded values to distribute can be ignored if they reached the top of the tree (this
    -- caller).

    reduceOneStep :: P18SFNumber -> (Bool, P18SFNumber)
    reduceOneStep currSFNumber
      | explodeDone = (True, newSFNumberE)
      | splitDone   = (True, newSFNumberS)
      | otherwise   = (False, currSFNumber)
      where
        (P18ReduceUp explodeDone newSFNumberE _ _) = explodeOrSplit Explode 0 currSFNumber
        (P18ReduceUp splitDone newSFNumberS _ _)   = explodeOrSplit Split 0 currSFNumber

    -- Do a single explode or split on a number, returning a boolean indicating whether one was
    -- done, the replacement number (if there was a change), and any values that need to be
    -- distribted to the left or right in the tree.

    explodeOrSplit :: P18Operation -> Int -> P18SFNumber -> P18ReduceUp

    -- Here we split a value that is greater than or equal to 10.

    explodeOrSplit operation depth currVal@(P18Value val)
      | depth > 4 = error "Depth greater than 4 with regular number in puzzle 18."

      -- Here this value is less than 10, so make no change, and nothing to distribute.
  
      | val < 10 || operation /= Split  = P18ReduceUp False currVal Nothing Nothing

      -- Create a new pair with this current number split in two.
  
      | otherwise = let newLeftVal  = val `quot` 2
                        newRightVal = val - newLeftVal
                        newLeftEle  = P18Value newLeftVal
                        newRightEle = P18Value newRightVal
                    in P18ReduceUp True (P18Pair newLeftEle newRightEle) Nothing Nothing

    -- Here we have a pair. If it is at a depth of 4, explode it, and otherwise, distribute any
    -- values from below due to deeper explosions.

    explodeOrSplit operation depth currPair@(P18Pair leftSide rightSide)
      | depth > 4 = error "Depth greater than 4 with pair number in puzzle 18."

      -- If we are at a depth of 4 and we are supposed to explode pairs on this pass, Explode this
      -- pair leaving a zero value her, and distributing the left and right upward.

      | depth == 4 && operation == Explode
        = P18ReduceUp True (P18Value 0) (getVal leftSide) (getVal rightSide)

      -- We did an explode and split on the left side of the pair, and there was a change
      -- there. Distribute any values to the right side of the pair, and send any values to
      -- distribute on the left up.

      | changeBelowL = let newRtSide
                             | isNothing distRightL = rightSide
                             | otherwise = addTo HugLeft (fromJust distRightL) rightSide
                       in  P18ReduceUp True (P18Pair newLeftSide newRtSide) distLeftL Nothing

      -- If there wasn't a change on the left side, then check the right side.

      | changeBelowR = let newLftSide
                             | isNothing distLeftR = leftSide
                             | otherwise = addTo HugRight (fromJust distLeftR) leftSide
                       in  P18ReduceUp True (P18Pair newLftSide newRightSide) Nothing distRightR

      -- No changes below and no exploding here, so return the current sub-tree.
  
      | otherwise = P18ReduceUp False currPair Nothing Nothing

      where
        (P18ReduceUp changeBelowL newLeftSide distLeftL distRightL)  = recurse leftSide
        (P18ReduceUp changeBelowR newRightSide distLeftR distRightR) = recurse rightSide

        recurse :: P18SFNumber -> P18ReduceUp
        recurse = explodeOrSplit operation nextDepth

        nextDepth = depth + 1

        -- Get the value associated with a literal number, and return in a Maybe. This should never
        -- be a pair, but generate an error if it is.

        getVal :: P18SFNumber -> Maybe Int
        getVal (P18Value val) = Just val
        getVal _ = error "Non value found at depth 4 in puzzle 18."

    -- Add the given value to the leftmost or rightmost place in the tree.

    addTo :: P18Direction -> Int -> P18SFNumber -> P18SFNumber
    addTo _ val (P18Value currVal) = P18Value (currVal + val)
    addTo dir val (P18Pair leftSide rightSide)
      | dir == HugLeft = P18Pair (addTo dir val leftSide) rightSide
      | otherwise = P18Pair leftSide (addTo dir val rightSide)

    -- Combine the two numbers together with addition, then reduce the result fully.

    addAndReduce :: P18SFNumber -> P18SFNumber -> P18SFNumber
    addAndReduce number1 number2 = fullyReduce (P18Pair number1 number2)

    -- Generate all subsets of length n of a given list.

    subsetsOfSize :: Int -> [a] -> [[a]]
    subsetsOfSize n xs
      | n < 1 = []
      | otherwise = subsetsOfSize' n (length xs) xs
      where
        subsetsOfSize' :: Int -> Int -> [b] -> [[b]]
        subsetsOfSize' 0 _ _ = [[]]
        subsetsOfSize' _ _ [] = []
        subsetsOfSize' n' len (y : ys)
          | n' > len = []
          | n' == len = [y : ys]
          | otherwise = subsetsOfSize' n' (len - 1) ys
                        ++ map(y :) (subsetsOfSize' (n' - 1) (len - 1) ys)

    -- Recursively parse a SnailFish number.

    parseSnailFishNumber :: Parser P18SFNumber
    parseSnailFishNumber = do
                             _ <- symbol "["
                             sfNum1 <- parseSnailFishNumber
                             _ <- symbol ","
                             sfNum2 <- parseSnailFishNumber
                             _ <- symbol "]"
                             return (P18Pair sfNum1 sfNum2)
                           <|> do
                             fmap P18Value integer

--
-- Code for Puzzle 19.
--

type P19Coords = [P19Coord]
type P19Coord = (Int, Int, Int)
type P19OrientAndCoord = (P19Coord, P19Orientation)
type P19SetNoConn = S.Set (Int, Int)

-- Orentations are described with an (x, y, z) coordinate system where if you are at the origin
-- facing the positive x direction, positive y will be on your left and positive z will be up. This
-- position is described as PosXR0. The PosX indicates that you are facing along the x-axis looking
-- in the positive direction. The R0 indicates no rotation, and each increment is 90 degrees
-- clockwise from the point of view of where you are facing.

data P19Orientation = PosXR0 | PosXR1 | PosXR2 | PosXR3 |
                      PosYR0 | PosYR1 | PosYR2 | PosYR3 |
                      PosZR0 | PosZR1 | PosZR2 | PosZR3 |
                      NegXR0 | NegXR1 | NegXR2 | NegXR3 |
                      NegYR0 | NegYR1 | NegYR2 | NegYR3 |
                      NegZR0 | NegZR1 | NegZR2 | NegZR3 deriving (Bounded, Enum, Eq, Show)

-- This data structure holds the scanner and associated information. That includes the ID, whether
-- it has been given a place or not, the orientation and location, which are PosXR0 and (0, 0, 0)
-- initially, and the coordinates that it found beacons. These coordinates will always be absolute,
-- so they are relative to the origin and PosXR0 orientation. When the scanner is initially created
-- but not placed, then these are relative to this scanner, but when the scanner is placed, then
-- they are adjusted to be still relative to PosXR0 and (0, 0, 0).

data P19Scanner = P19Scanner { _id             :: Int
                             , _placed         :: Bool
                             , _placeAndOrient :: P19OrientAndCoord
                             , _coordCount     :: Int
                             , _coords         :: P19Coords
                             , _coordSet       :: Maybe (S.Set P19Coord)
                             } deriving Show
type P19Scanners = [P19Scanner]

puzzle_19 :: IO (Int, Int)
puzzle_19 = do
  parseRes <- fmap (parse parseScanners) (readFile "puzzle_19.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 19.")

  -- Make sure each scanner sees enough beacons to connect them, given the number of matches needed.

  let initScannerData = (fst . head) parseRes
      enoughBeacons = all ((>= matchesNeeded) . length . _coords) initScannerData

  unless enoughBeacons
       (ioError $ userError "At least one scanner doesn't see enough beacons to match.")

  -- Find connections between scanners, using scanner 0 as the root scanner located at (0, 0, 0) and
  -- with the default orientation.

  let connectedSetM = connectScanners S.empty [] initScannerData

  when (isNothing connectedSetM)
       (ioError $ userError "No consistent connected set of scanners found.")

  -- Count the number of unique beacons and the maximum Manhattan distance between any two scanners.

  let connectedSet = fromJust connectedSetM
      beaconCount = (S.size . foldl' S.union S.empty . map (S.fromList . _coords)) connectedSet
      maxManhattanDist = maximum $ computeManhattanDist <$> connectedSet <*> connectedSet

  return (beaconCount, maxManhattanDist)

  where

    -- From the problem definition.

    matchesNeeded = 12

    initialLocAndOrient = ((0, 0, 0), PosXR0)

    -- Given two scanners that have been placed, compute the Manhattan distance between them.

    computeManhattanDist :: P19Scanner -> P19Scanner -> Int
    computeManhattanDist scanner1 scanner2 = (sum . map abs) [x1 - x2, y1 - y2, z1 - z2]
      where
        [(x1, y1, z1), (x2, y2, z2)] = map (fst . _placeAndOrient) [scanner1, scanner2]

    -- Given at least one scanner in the connected list, work to connect each of the unconnected
    -- ones to one of the scanners in the connected list. Ultimately, either we can connect all of
    -- them, in which case return that list of connected scanners with their locked locations and
    -- orientations, or return Nothing.

    connectScanners :: P19SetNoConn -> P19Scanners -> P19Scanners -> Maybe P19Scanners

    -- If we have found locations for all of the unconnected scanners, then return a valid set of
    -- connected ones.  If there are no scanners to begin with, we still have a valid set of
    -- scanners, which is empty.

    connectScanners _ currConnectedSet [] = Just currConnectedSet

    -- If we haven't placed any scanners yet, but we have unplaced ones, place the first of them at
    -- the origin with no orientation change.

    connectScanners noConnSet [] (firstScanner : restOfScanners)
      = connectScanners noConnSet [rootScanner] restOfScanners
      where
        rootScanner = firstScanner {_placed = True, _coordSet = Just coordSet}
        coordSet = S.fromList (_coords firstScanner)

    -- Here we look for any scanner in the unconnected list that can align with any one in the
    -- connected list. If found. continue with the matching process, otherwise it is an error.

    connectScanners noConnSet currConnectedSet unconnectedScanners
      | isNothing validScannerAndRestM = Nothing
      | otherwise = let (placedAndOriented, remainUnconnected) = fromJust validScannerAndRestM
                        newConnectedSet = newNoConnSet `seq` placedAndOriented : currConnectedSet
                    in  connectScanners newNoConnSet newConnectedSet remainUnconnected
      where

        (newNoConnSet, validScannerAndRestM) = lookForConn noConnSet [] unconnectedScanners

        -- Go through the unconnected scanners and look for one that connects to one of the current
        -- connected set. If none do, then return Nothing, but if one does, return it along with the
        -- other unconnected ones.

        lookForConn :: P19SetNoConn -> P19Scanners -> P19Scanners
                       -> (P19SetNoConn, Maybe (P19Scanner, P19Scanners))
        lookForConn noConnSet' _ [] = (noConnSet', Nothing)
        lookForConn noConnSet' triedAndFailed (scannerToTry : restToTry)
          | null placements = let newNoConnSet' = foldl' addToSet noConnSet' currConnToCheck
                                  addToSet accSet sc = S.insert (currScannerID, _id sc) accSet
                              in lookForConn newNoConnSet' (scannerToTry : triedAndFailed) restToTry
          | otherwise = (noConnSet', Just (head placements, restToTry ++ triedAndFailed))
          where

            -- This list will be empty if the scanner to try doesn't connect with any of the
            -- scanners in the connected list. If it does, it may connect with one or more, but if
            -- more than one, they will cause it to be located and oriented in exactly the same way.

            placements = mapMaybe (connectIfPossible scannerToTry) currConnToCheck
            currConnToCheck = filter haventCheckedThisPair currConnectedSet
            currScannerID = _id scannerToTry
            haventCheckedThisPair sc = S.notMember (currScannerID, _id sc) noConnSet'

    -- The first scanner has an undefined origin and orientation. The second one is defined. See if
    -- the first one can be placed at a location and with an orientation such that at least 12
    -- beacons have matching locations between the two. If so, return the first scanner object with
    -- the location and orientation for that to be true. If no such match is found, then return
    -- Nothing.

    connectIfPossible :: P19Scanner -> P19Scanner -> Maybe P19Scanner
    connectIfPossible floatScanner lockedScanner = foldr accMatch Nothing allPossFirstBeaconMatches
      where

        -- Each locked scanner has a set of the absolute beacon coordinates that it can see.  Also,
        -- get the list of locked coordinates that are needed to use to orient the floating
        -- scanner. We don't need to try all of them, just enough that the rest of the matched
        -- beacons would be the rest of the list.

        lockedAllSet = fromMaybe S.empty (_coordSet lockedScanner)
        coordsToConsiderForLocked = getNeededCoords lockedScanner

        -- If there are enough matching beacon coordinates with the locked scanner given this
        -- orientation and scanner location, then return the scanner, otherwise return the
        -- accumulator, which will be Nothing if none of the possibilities in the list work.

        accMatch :: P19Scanner -> Maybe P19Scanner -> Maybe P19Scanner
        accMatch currScanner acc
          | coincideCount >= matchesNeeded = Just currScanner
          | otherwise = acc
          where
            coincideCount = (length . filter id . map inLockedCoordSet) (_coords currScanner)
            inLockedCoordSet = flip S.member lockedAllSet

        -- When orienting the floating scanner based on pair of coordinates between the floating
        -- scanner and the locked scanner, we don't need to check all pairs, just enough that we
        -- would be guaranteed to see one of the needed matches. This is used to just take those
        -- that we need to check off the front of the list.

        getNeededCoords :: P19Scanner -> P19Coords
        getNeededCoords scanner = take resultLen (_coords scanner)
          where
            resultLen = _coordCount scanner - matchesNeeded + 1
    
        allPossFirstBeaconMatches :: [P19Scanner]
        allPossFirstBeaconMatches
          = concat $ combineFAndLCoords <$> coordsToConsiderForLocked <*> coordsToConsiderForFloat
          where
            coordsToConsiderForFloat = getNeededCoords floatScanner

            combineFAndLCoords :: P19Coord -> P19Coord -> [P19Scanner]
            combineFAndLCoords lockedCoord floatCoord = map scannerGivenOrientAndBeacon [minBound..maxBound]
              where

                -- Return a scanner with the given orientation and a location that aligns the locked
                -- coord beacon and the floating coord beacon.

                scannerGivenOrientAndBeacon :: P19Orientation -> P19Scanner
                scannerGivenOrientAndBeacon orient
                  = placeScannerToAlignBeacon floatScanner orient lockedCoord floatCoord

    -- Given an unplaced scanner, place it given an orientation, a locked coordinate, and an
    -- unlocked one so that the two coordinates coincide when the floating one is converted to
    -- absolute.
 
    placeScannerToAlignBeacon :: P19Scanner -> P19Orientation -> P19Coord -> P19Coord -> P19Scanner
    placeScannerToAlignBeacon unplacedScanner newOrient (xL, yL, zL) floatBeaconCoord
      | _placed unplacedScanner = error "Trying to place an already placed scanner."
      | otherwise = newCoordAndOrient `seq` placeScannerAndMakeCoordsAbsolute convFn unplacedScanner newCoordAndOrient
      where
        newCoordAndOrient = newScannerCoord `seq` (newScannerCoord, newOrient)
        newScannerCoord = newX `seq` (newX, newY, newZ)
        newX = newY `seq` xL - xNO
        newY = newZ `seq` yL - yNO
        newZ = zL - zNO
        (xNO, yNO, zNO) = convFn floatBeaconCoord
        convFn = convertFromNeutralOrientationFn newOrient

    -- Given an unplaced scanner, place it given a new scanner location and orientation. Convert
    -- all beacon coordinates to be absolute, and create a set of these coordinates as well.

    placeScannerAndMakeCoordsAbsolute :: (P19Coord -> P19Coord) -> P19Scanner -> P19OrientAndCoord -> P19Scanner
    placeScannerAndMakeCoordsAbsolute convFn unplacedScanner newLocAndOrient = newScanner
      where
        newScanner = P19Scanner (_id unplacedScanner) True newLocAndOrient
                                (_coordCount unplacedScanner) newBeaconCoords
                                (Just (S.fromList newBeaconCoords))
        newBeaconCoords = map (convertToAbsoluteFn convFn (fst  newLocAndOrient)) (_coords unplacedScanner)
        
    -- Convert the coordinate to an absolute coordinate given the scanner's new location and
    -- orientation. The scanner must have been just placed, and the original coordinate was the
    -- origin.

    convertToAbsoluteFn :: (P19Coord -> P19Coord) -> P19Coord -> P19Coord -> P19Coord
    convertToAbsoluteFn convFn (sX, sY, sZ) coord = let (oX, oY, oZ) = convFn coord
                                                        newX = sX + oX
                                                        newY = newX `seq` sY + oY
                                                        newZ = newY `seq` sZ + oZ
                                                    in newZ `seq` (newX, newY, newZ)

    convertFromNeutralOrientationFn :: P19Orientation -> (P19Coord -> P19Coord)
    convertFromNeutralOrientationFn orient
      = case orient of
          PosXR0 -> (\(x, y, z) -> (x, y, z))
          PosXR1 -> (\(x, y, z) -> (x, z, -y))
          PosXR2 -> (\(x, y, z) -> (x, -y, -z))
          PosXR3 -> (\(x, y, z) -> (x, -z, y))
          PosYR0 -> (\(x, y, z) -> (y,-x , z))
          PosYR1 -> (\(x, y, z) -> (y, z, x))
          PosYR2 -> (\(x, y, z) -> (y, x, -z))
          PosYR3 -> (\(x, y, z) -> (y, -z, -x))
          PosZR0 -> (\(x, y, z) -> (z, y, -x))
          PosZR1 -> (\(x, y, z) -> (z, -x, -y))
          PosZR2 -> (\(x, y, z) -> (z, -y, x))
          PosZR3 -> (\(x, y, z) -> (z, x, y))
          NegXR0 -> (\(x, y, z) -> (-x, -y, z))
          NegXR1 -> (\(x, y, z) -> (-x, z, y))
          NegXR2 -> (\(x, y, z) -> (-x, y, -z))
          NegXR3 -> (\(x, y, z) -> (-x, -z, -y))
          NegYR0 -> (\(x, y, z) -> (-y, x, z))
          NegYR1 -> (\(x, y, z) -> (-y, z, -x))
          NegYR2 -> (\(x, y, z) -> (-y, -x, -z))
          NegYR3 -> (\(x, y, z) -> (-y, -z, x))
          NegZR0 -> (\(x, y, z) -> (-z, y, x))
          NegZR1 -> (\(x, y, z) -> (-z, x, -y))
          NegZR2 -> (\(x, y, z) -> (-z, -y, -x))
          NegZR3 -> (\(x, y, z) -> (-z, -x, y))

    -- Parse all of the scanner descriptions and return them in a list.

    parseScanners :: Parser P19Scanners
    parseScanners = some parseScanner

    -- Parse an individual scanner with its ID and the beacon coordinates it detects.

    parseScanner :: Parser P19Scanner
    parseScanner = do
      space
      skipDashes
      space
      _ <- symbol "scanner"
      space
      idNum <- int
      space
      skipDashes
      space
      coords <- some parseCoord
      return (P19Scanner idNum False initialLocAndOrient (length coords) coords Nothing)
      where
        skipDashes :: Parser ()
        skipDashes = do
          _ <- many (sat (== '-'))
          return ()

    -- Parse a coordinate triple.

    parseCoord :: Parser P19Coord
    parseCoord = do
      x <- int
      _ <- symbol ","
      y <- int
      _ <- symbol ","
      z <- int
      _ <- space
      return (x, y, z)

--
-- Code for Puzzle 20.
--

-- Types to hold an image state and the algorithm array for problem 20. We also need to account for
-- when a zero index (all 9 squares empty) indicates a turned on pixel, because that will cause the
-- infinite plane of pixels to alternate on/off if the last algorithm entry is for pixel off.

data P20PixelState  = PixOn | PixOff deriving Eq
data P20PixelSeq    = AlwaysOn | AlwaysOff | Alternating deriving Eq
type P20AlgArr      = UA.Array Int Int
type P20ImageMatrix = UA.Array (Int, Int) Int
data P20ImageState  = P20ImageState { _currPixState :: P20PixelState
                                    , _pixelSeq     :: P20PixelSeq
                                    , _currMatrix   :: P20ImageMatrix
                                    }

puzzle_20 :: IO (Int, Int)
puzzle_20 = do

  let algElements = 512
  
  parseRes <- fmap (parse (parseAlgAndImage algElements)) (readFile "puzzle_20.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 20.")

  -- Read the algorithm array and the initial image state. Create a list of image iterations, and
  -- then select the second one for the first part and the 50th for the second.

  let (algArr, imageMatrix) = (fst . head) parseRes
      successivelyEnhancedImages = iterate (enhanceImage algArr) imageMatrix
      [part1Ans, part2Ans] = map (countOnElements . (successivelyEnhancedImages !!)) [2, 50]

  return (part1Ans, part2Ans)

  where

    -- Apply the image enhancement algorithm to the input and return the result, which will be
    -- expanded by one in each direction.

    enhanceImage :: P20AlgArr -> P20ImageState -> P20ImageState
    enhanceImage algArray (P20ImageState currPixState pixelSeq currImage) = resultImage
      where

        -- The resulting image will be one larger in every direction. The pixel sequence for the
        -- area outside the image will always be the same as the input. The pixel state for those
        -- outside the image may alternate each enhancement or it may stay the same.

        resultImage = P20ImageState newPixState pixelSeq (UA.array newB initList)
        newPixState
          | pixelSeq == AlwaysOn  = PixOn
          | pixelSeq == AlwaysOff = PixOff
          | otherwise = if currPixState == PixOn then PixOff else PixOn

        -- Create the new array one larger in each direction, then populate based on the genPixel
        -- function for each spot.

        initList = [((x, y), genPixel (x, y)) | x <- [newMinX..newMaxX], y <- [newMinY..newMaxY]]
        newB@((newMinX, newMinY), (newMaxX, newMaxY)) = ((minX - 1, minY - 1), (maxX + 1, maxY + 1))
        ((minX, minY), (maxX, maxY)) = UA.bounds currImage

        -- Generate the appropriate pixel for each grid location. This will look at the surrounding
        -- pixels in the current image as well as the current state for the area outside the image.

        genPixel :: (Int, Int) -> Int
        genPixel (x, y) = algArray UA.! algIndex
          where
            algIndex = foldl' includeCurrValInIndex 0 blockOfMatrixLocs
            blockOfMatrixLocs = [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1), (x - 1, y), (x, y),
                                 (x + 1, y), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
            includeCurrValInIndex :: Int -> (Int, Int) -> Int
            includeCurrValInIndex accInd loc@(xVal, yVal)
              | locOutOfBounds = shiftedAcc .|. (if currPixState == PixOn then 1 else 0)
              | otherwise = shiftedAcc .|. (currImage UA.! loc)
              where
                shiftedAcc = shiftL accInd 1
                locOutOfBounds = xVal < minX || xVal > maxX || yVal < minY || yVal > maxY

    countOnElements :: P20ImageState -> Int
    countOnElements (P20ImageState _ _ currMatrix) = (length . filter (== 1) . UA.elems) currMatrix

    -- Parse the input.

    parseAlgAndImage :: Int -> Parser (P20AlgArr, P20ImageState)
    parseAlgAndImage algElements = do

      -- Read in the image enhancement algorithm, and insure that it is 512 characters long. Convert
      -- the list of characters to an array of int values, 1 for '#' and 0 for '.'. Note that
      -- parseImageLine consumes the space after the initial string.

      algChars <- parseImageLine

      let algCharLen = length algChars
          algArr = UA.listArray (0, algCharLen - 1) (map convToInt algChars)

      guard $ algCharLen == algElements

      -- Read in the image as a list of strings. Make sure there is at least one line with one
      -- character, and insure that all lines are the same length.  Note that matrixMaxHeight and
      -- matrixMaxWidth are both one less than the actual number and are used as the maximum array
      -- indices.

      imageLineList <- some parseImageLine

      let matrixMaxHeight = length imageLineList - 1

      guard $ matrixMaxHeight >= 0

      -- Create a matrix with 1 representing '#' and 0 representing '.' and also do some error
      -- checking on the input.

      let matrixMaxWidth = length (head imageLineList) - 1
          allSameWidth = all ((== matrixMaxWidth) . subtract 1 . length) imageLineList
          indices = [(x, y) | y <- [0..matrixMaxHeight], x <- [0..matrixMaxWidth]]
          matrixTF = (map convToInt . concat . reverse) imageLineList
          matrixArr = UA.array ((0, 0), (matrixMaxWidth, matrixMaxHeight))
                               (zip indices matrixTF)

      -- Make sure the initial image is a rectangle.

      guard allSameWidth

      -- Determine the state of the elements outside of the pattern.

      let pixelSeq
            | algArr UA.! 0 == 0 = AlwaysOff
            | algArr UA.! (algElements - 1) == 0 = Alternating
            | otherwise = AlwaysOn

      return (algArr, P20ImageState PixOff pixelSeq matrixArr)

    -- Parse a single line of '.' and '#' characters, and consume any following space characters.

    parseImageLine :: Parser String
    parseImageLine = do
      str <- some (sat (\ch -> ch == '.' || ch == '#'))
      _ <- space
      return str

    -- Convert a '.' to 0 and a '#' to 1.

    convToInt :: Char -> Int
    convToInt ch = if ch == '.' then 0 else 1

    -- Generate a string for the given image, displayed like the given problem. Prefixed by '_' so
    -- the compiler doesn't complain about it being an unused function.

    _genImageStr :: P20ImageState -> String
    _genImageStr imageState = foldr showRow [] [maxY,maxY-1..minY]
      where
        currImage = _currMatrix imageState
        ((minX, minY), (maxX, maxY)) = UA.bounds currImage

        showRow :: Int -> String -> String
        showRow y accStr = thisRowStr ++ accStr
          where
            thisRowStr = foldr showPixel "\n" [minX..maxX]
            showPixel :: Int -> String -> String
            showPixel x accRowStr = (if pix == 0 then '.' else '#') : accRowStr
              where
                pix = currImage UA.! (x, y)

--
-- Code for Puzzle 21.
--

-- This problem requires a larger integer type than Int for 32-bit machines.

type P21ResultType = Integer

-- There are two players of the game.

data P21Player = Player1 | Player2 deriving (Eq, Ord, Show)

-- Each player has a current location and a score accumulated through the game.

data P21PlayerState = P21PlayerState { _loc   :: Int
                                     , _score :: P21ResultType
                                     } deriving (Eq, Ord, Show)

-- The game state holds who is to play next, the states of the two players, and the number of ways
-- to get to this state (for the quantum die case).

data P21GameState = P21GameState { _toPlayB       :: P21Player
                                 , _player1State  :: P21PlayerState
                                 , _player2State  :: P21PlayerState
                                 , _occurrenceCnt :: P21ResultType
                                 } deriving (Show, Ord, Eq)

-- The list of dice sums possible after 3 rolls along with the number of paths to get there. For
-- normal dice the count is always 1.

type P21DiceSumsAndCounts = [(Int, P21ResultType)]

-- Model the two different types of dice.

data P21DiceState = P21NormalDie Int P21ResultType | P21QuantumDie P21ResultType

-- Returns the number of times the die has been rolled so far in this game.

diceRollCount :: P21DiceState -> P21ResultType
diceRollCount (P21NormalDie _ count) = count
diceRollCount (P21QuantumDie count) = count

-- Return the new state of the die after rolling it three times, along with the sum of those three
-- rolls. We return 1 for the paths to get to this roll sum, since this is not the quantum case.

rollDiceThrice :: P21DiceState -> (P21DiceSumsAndCounts, P21DiceState)
rollDiceThrice (P21NormalDie currVal count) = ([(sumOf3, 1)], newDieState)
  where
    newDieState = P21NormalDie thirdValAfter newCount
    sumOf3 = sum threeRolls
    newCount = count + 3
    thirdValAfter = head nextRolls
    (threeRolls, nextRolls) = (splitAt 3 . iterate incDie) currVal

    incDie :: Int -> Int
    incDie dieVal = if dieVal == 100 then 1 else dieVal + 1

-- When we roll the quantum die, we roll it three times and return a list of all possible sums of
-- the three dice along with the number of ways of getting to each of those sums. Increment the
-- count by 3.

rollDiceThrice (P21QuantumDie count) = newDieState `seq` (threeDicePossibilities, newDieState)
  where
    newDieState = newCount `seq` P21QuantumDie newCount
    newCount = count + 3

    -- Here we list all of the possible dice sums (the first in the pair) for three quantum dice
    -- along with the number of different ways that sum can be achieved (the second in the pair).

    threeDicePossibilities :: P21DiceSumsAndCounts
    threeDicePossibilities = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

-- For the quantum die game we need to keep track of the number of accumulated wins for each player
-- as well as the set of possible game states after a particular number of turns. We also keep the
-- list of games won by each player in the last turn. This is used to compute the answer for part 1,
-- and could probably be simplified.

data P21CountsAndGameStates = P21CountsAndGameStates { _winCounts   :: (P21ResultType, P21ResultType)
                                                     , _dieState    :: P21DiceState
                                                     , _activeGames :: [P21GameState]
                                                     , _p1JustWon   :: [P21GameState]
                                                     , _p2JustWon   :: [P21GameState]
                                                     }

puzzle_21 :: IO (P21ResultType, P21ResultType)
puzzle_21 = do
  parseRes <- fmap (parse parseStartPositions) (readFile "puzzle_21.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 21.")

  -- Read the algorithm array and the initial image state. Create a list of image iterations, and
  -- then select the second one for the first part and the 50th for the second.

  let (p1Start, p2Start) = (fst . head) parseRes

  -- Both parts of the puzzle use the same initial game state.
  
  let initPlayer1   = P21PlayerState p1Start 0
      initPlayer2   = P21PlayerState p2Start 0
      initGameState = [P21GameState Player1 initPlayer1 initPlayer2 1]

  -- For the first part of the problem, begin at the start state with a normal die, and take turns
  -- until one side wins, then compute the answer from the data at the winning state. Because I used
  -- the same code for both parts, getting the loser's score is a little awkward here.

  let initialStatePart1 = P21CountsAndGameStates (0, 0) (P21NormalDie 1 0) initGameState [] []
      successiveNormalStates  = iterate (stepStatesAndWins isWin1) initialStatePart1
      finalState = (head . dropWhile ((not . null) . _activeGames)) successiveNormalStates
      diceRolls = (diceRollCount . _dieState) finalState
      p1JustWon = _p1JustWon finalState
      p2JustWon = _p2JustWon finalState
      loserScore = if null p1JustWon then (_score . _player1State . head)  p2JustWon
                   else (_score . _player2State . head)  p1JustWon
      ans1 = diceRolls * loserScore

  -- For the second part of the problem, walk out all possible states until all paths have reached a
  -- winning state. Accumulate the wins by player as we move through successive turns, and weed out
  -- those winning states at the same time.
 
  let initialStatePart2 = P21CountsAndGameStates (0, 0) (P21QuantumDie 0) initGameState [] []
      successiveQuantumStates = iterate (stepStatesAndWins isWin2) initialStatePart2
      ans2 = (uncurry max . _winCounts . head . dropWhile ((not . null) . _activeGames))
             successiveQuantumStates

  return (ans1, ans2)

  where

    -- Call the function to update the list of game states, then filter out any winning states for
    -- either player, returning them as just won states and also counting the winning states for
    -- each to add to the total winning counts.

    stepStatesAndWins :: (P21PlayerState -> Bool) -> P21CountsAndGameStates -> P21CountsAndGameStates
    stepStatesAndWins isWinFn (P21CountsAndGameStates (p1WinCnt, p2WinCnt) dieState gameStates _ _)
      = winningCounts `seq` P21CountsAndGameStates winningCounts newDieState notWins p1Wins p2Wins
      where
        winningCounts = newP1Count `seq` newP2Count `seq` (newP1Count, newP2Count)
        newP1Count = p1WinCnt + sumCounts p1Wins
        newP2Count = p2WinCnt + sumCounts p2Wins
        (p1Wins, others)  = partition (isWinFn . _player1State) newGameStates
        (p2Wins, notWins) = partition (isWinFn . _player2State) others
        newGameStates = stepForAllPoss dieRollData gameStates
        (dieRollData, newDieState) = rollDiceThrice dieState

    -- Given a list of unfinished game states (with counts for the number of ways to reach this
    -- state) and a list of quantum dice roll data, generate the list of resulting game states
    -- merging common states and keeping track of path counts.
    
    stepForAllPoss :: P21DiceSumsAndCounts -> [P21GameState] -> [P21GameState]
    stepForAllPoss dieRollData
      = map combineCounts . groupBy eqFn . sortBy cmpFn . concatMap (makeQuantumTurns dieRollData)
      where

        -- Two states are considered equal if everything but the occurrence count is equal.

        eqFn :: P21GameState -> P21GameState -> Bool
        eqFn state1 state2 = cmpFn state1 state2 == EQ

        -- Compare two game states. The comparison order is player next, player one, player two. We
        -- don't compare the occurrence count because we want to add the occurrence counts together
        -- for states that are identical otherwise.

        cmpFn :: P21GameState -> P21GameState -> Ordering
        cmpFn state1 state2
          | toPlayCmp  /= EQ = toPlayCmp
          | player1Cmp /= EQ = player1Cmp
          | otherwise = player2Cmp
          where
            toPlayCmp  = compare (_toPlayB state1) (_toPlayB state2)
            player1Cmp = compare (_player1State state1) (_player1State state2)
            player2Cmp = compare (_player2State state1) (_player2State state2)

        -- Given a list of game states (and when this is called, all of the states will be identical
        -- except for the occurrence counts), add together the occurrence counts and use the head of
        -- the list to create a single state with the sum of the occurrence counts.

        combineCounts :: [P21GameState] -> P21GameState
        combineCounts xs = (head xs) { _occurrenceCnt = sumCounts xs }

    -- Return the sum of the occurrence counts of all of the states in the list.

    sumCounts :: [P21GameState] -> P21ResultType
    sumCounts = sum . map _occurrenceCnt

    -- Define the winning threshold for each part of this problem, and instantiate win functions for
    -- each as well, based on the given thresholds.

    isWin1 = isWin winThreshold1
    isWin2 = isWin winThreshold2
    winThreshold1 = 1000
    winThreshold2 = 21

    -- Given the winning threshold, is this player state a winning one.

    isWin :: P21ResultType -> P21PlayerState -> Bool
    isWin winThreshold playerState = _score playerState >= winThreshold

    -- Given a game state, generate the list of game states arrived at by rolling and summing three
    -- quantum dice.

    makeQuantumTurns :: P21DiceSumsAndCounts -> P21GameState -> [P21GameState]
    makeQuantumTurns threeDicePoss gameState = map (takeTurn gameState) threeDicePoss

    -- Update the status of the player whose turn it is by the given roll and count of ways to
    -- achieve that roll. Note that the seqs used here to make the values strict improves
    -- performance notably.

    takeTurn :: P21GameState -> (Int, P21ResultType) -> P21GameState
    takeTurn (P21GameState toPlay player1State player2State occCount) (threeDice, pathCount)
      = newCount `seq` P21GameState newPlayer newPlayer1State newPlayer2State newCount
      where
        newCount = newPlayer1State `seq` newPlayer2State `seq` occCount * pathCount
        (newPlayer, newPlayer1State, newPlayer2State)
          | toPlay == Player1 = (Player2, makeMove threeDice player1State, player2State)
          | otherwise = (Player1, player1State, makeMove threeDice player2State)

    -- Make a move from the dice sum given for the player. We make sure the location stays in the
    -- range it needs to by wrapping around after adding the dice amount.

    makeMove :: Int -> P21PlayerState -> P21PlayerState
    makeMove threeDice (P21PlayerState loc score) = newScore `seq` P21PlayerState newLoc newScore
      where
        newLoc = (loc + threeDice - 1) `rem` 10 + 1
        newScore = fromIntegral newLoc + score

    -- Parse the input.

    parseStartPositions :: Parser (Int, Int)
    parseStartPositions = do
      p1Start <- parsePosition
      _ <- space
      p2Start <- parsePosition
      return (p1Start, p2Start)
      where
        parsePosition :: Parser Int
        parsePosition = do
          _ <- some alphanumSp
          _ <- symbol ":"
          _ <- space
          int

--
-- Code for Puzzle 22.
--

type P22IndexRange = (Int, Int)
data P22CubeData = P22CubeData { d_on     :: Bool
                               , d_xRange :: P22IndexRange
                               , d_yRange :: P22IndexRange
                               , d_zRange :: P22IndexRange
                               } deriving Show
--data P22CubeTree = P22CubeTree { d_cubeData :: P22CubeData
--                               , d_subCubes :: [P22CubeTree]
--                               } deriving Show
--
---- Intersection characteristics from the point of view of the cube relative to another cube.
--
--data P22IntersectSummary = NoIntersection | ProperSubCube | Identical | SuperCube | ItsComplicated deriving Show
--data P22IntersectDetail = LowerLower | LowerIn | LowerHigher | InIn | InHigher | HigherHigher deriving (Show, Eq)
--data P22IntersectType = P22IntersectType { d_summary :: P22IntersectSummary
--                                         , d_xRel    :: P22IntersectDetail
--                                         , d_yRel    :: P22IntersectDetail
--                                         , d_zRel    :: P22IntersectDetail
--                                         } deriving Show
--data P22IntersectType = NoIntersection | ProperSubCube | Identical | SuperCube | Partial deriving Show
--
--puzzle_22a :: IO (Int, Int)
--puzzle_22a = do
--  parseRes <- fmap (parse parseCubeRanges) (readFile "puzzle_22.inp")
--
--  -- Standard error check to see that there was exactly one full parser interpretation of the input.
--  
--  when (badParseOfInput parseRes)
--       (ioError $ userError "Parse error in input for puzzle 22.")
--
--  -- Read the list of cube definitions.
--
--  let cubeDefinitions = (fst . head) parseRes
--      xRange1@(xLow, xHigh) = (-50, 50)
--      yRange1@(yLow, yHigh) = (-50, 50)
--      zRange1@(zLow, zHigh) = (-50, 50)
--      initialCube1 = P22CubeData False xRange1 yRange1 zRange1
--      cubeDefsInRange = filter inRangeBounds cubeDefinitions
--      finalCubeTree1 = foldl' addCubeToTreeA initialCube1 cubeDefsInRange
--      totalOn1 = countOn finalCubeTree1
--
--      -- Find the full extent of the input data range in terms of X,Y,Z pairs..
--
--      initialBounds2 = replicate 3 (maxBound, minBound)
--      fullBounds = foldl' accMaxBounds initialBounds2 cubeDefinitions
----      finalArr :: UA.Array (Int, Int, Int) Bool
----      assocList = (concatMap switchAllCubeElements . filter inRangeBounds) cubeDefinitions
----      finalArr = UA.accumArray (flip const) False ((xLow, yLow, zLow), (xHigh, yHigh, zHigh))
----                 assocList
----      onCount = (length . filter id . UA.elems) finalArr
--
----  putStrLn $ mconcat ["Cube Defs: ", show cubeDefinitions]
--  putStrLn $ mconcat ["Final on count: ", show onCount]
--
--  return (43, 43)
--  where
--
--    -- Return the low/high pair that contains the lowest low and highest high of the two.
--
--    accMaxBounds accBoundsXYZ currCube
--      = zipWith extremeBounds accBoundsXYZ rangesXYZ
--      where
--        rangesXYZ = rangesToListOfThree currCube
--        extremeBounds (low1, high1) (low2, high2) = lowResult `seq` (lowResult, highResult)
--          where
--            lowResult  = highResult `seq` min low1 low2
--            highResult = max high1 high2
--
--    -- Given a cube and a cube tree, add the cube to the cube tree and return the result. The cube
--    -- tree is a recursive structure holding contained smaller and distict sub-cubes (rectangles,
--    -- actually).
--
--    addCubeToTreeA :: P22CubeData -> P22CubeTree -> P22CubeTree
--    addCubeToTreeA cubeData treeData = P22CubeTree cubeData newSubTrees
--      where
--        newSubTrees = addToAndAdjustSubCubes sameState [] (d_subTrees treeData)
--        sameState   = d_on cubeData == d_on tCubeData
--        tCubeData   = d_cubeData treeData
--
--        addToAndAdjustSubCubes :: Bool -> [P22CubeTree] -> [P22CubeTree] -> [P22CubeTree]
--
--        -- If there are no sub-cubes, then just add this one unless it has the same on-state as the
--        -- cube tree we're adding to.
--
--        addToAndAdjustSubCubes dontAddThisCube alreadyProcessed []
--          | dontAddThisCube = alreadyProcessed
--          | otherwise = convCubeDataToCubeTree cubeData : alreadyProcessed
--
--        addToAndAdjustSubCubes dontAddThisCube alreadyProcessed (currCubeTree : remainCubeTrees)
--
--          -- If the cube doesn't intersect with the cube tree, then there is nothing to do, so move
--          -- on to the next cube tree.
--
--          | intersectSummary == NoIntersection
--            = addToAndAdjustSubCubes dontAddThisCube (currCubeTree : alreadyProcessed) remainCubeTrees
--
--          -- If the cube is entirely inside this sub-cube, merge it in with this sub-cube and return
--          -- it as a replacement for currCubeTree. There isn't any need to look farther down the
--          -- sub-cube list because since they are disjoint, this cube can't intersect any others.
--
--          | intersectSummary == ProperSubCube
--            = let mergedSubCube = addCubeToTreeA cubeData currCubeTree
--              in  mergedSubCube : (alreadyProcessed ++ remainCubeTrees)
--
--          -- If this cube is identical to the current sub-cube, then either replace it or drop it,
--          -- depending on whether this cube has the same on-state as the parent cube.
--
--          | intersectSummary == Identical
--            = if dontAddThisCube then alreadyProcessed ++ remainCubeTrees
--              else let replacementCube = convCubeDataToCubeTree cubeData
--                   in  replacementCube : (alreadyProcessed ++ remainCubeTrees)
--
--          -- If the added cube entirely covers, and is larger, than the current sub-cube, just drop
--          -- the sub-cube and move on.
--
--          | intersectSummary == SuperCube
--            = addToAndAdjustSubCubes dontAddThisCube alreadyProcessed remainCubeTrees
--
--          -- Here is the complicated case. Here there is a partial intersection. We clip the
--          -- cube-tree and return the part(s) left over after chopping out the cube. We then have to
--          -- move on in the sub-tree list because the cube we're adding protruded from the sub-cube
--          -- and may intersect one or more others.
--
--          | otherwise
--            = let cubePieces = clipIntersectionFromCubeTree cubeData currCubeTree
--              in  addToAndAdjustSubCubes dontAddThisCube (cubePieces ++ alreadyProcessed) remainCubeTrees
--          where
--            (intersectSummary, xDetail, yDetail, zDetail) = calcIntersectA cubeData (d_cubeData currCubeTree)
--
--    clipIntersectionFromCubeTree :: P22CubeData -> P22CubeTree -> [P22CubeTree]
--    clipIntersectionFromCubeTree cubeData treeData = undefined
--
--    -- Given two cubes, determine if they intersect or not, and if so, in what way.  The result is
--    -- defined from the perspective of the first cube and separately for x, y, and z, and there is a
--    -- summary enum for common cases.
--
--    calcIntersectA :: P22CubeData -> P22CubeData -> P22IntersectType
--    calcIntersectA refCube otherCube = P22IntersectType summaryVal xRelation yRelation zRelation
--      where
--        summaryVal
--          | any (\r -> r == BelowBelow || r == AboveAbove) relats = NoIntersection
--          | refRanges == otherRanges = Identical
--          | all (== WithinWithin) relats = ProperSubCube
--          | 
--        relats@[xRelation, yRelation, zRelation] = zipWith computeRelation refRanges otherRanges
--        refRanges   = rangesToListOfThree refCube
--        otherRanges = rangesToListOfThree otherCube
--        computeRelation :: P22IndexRange -> P22IndexRange -> P22IntersectDetail
--        computeRelation (lowRef, highRef) (lowOther, highOther)
--          | highRef < lowOther = BelowBelow
--          | lowRef > highOther = AboveAbove
--          | lowRef < lowOther  = if highRef <= highOther then BelowWithin else BelowAbove
--          | otherwise = if highRef <= highOther then WithinWithin else WithinAbove
--
--    -- Convert a cube data to a cube tree with no sub-cube trees.
--
--    convCubeDataToCubeTree :: P22CubeData -> P22CubeTree
--    convCubeDataToCubeTree cubeData = P22CubeTree cubeData []
--
--    rangesToListOfThree :: P22CubeData -> [P22IndexRange]
--    rangesToListOfThree (P22CubeData _ currX currY currZ) = [currX, currY, currZ]
--
--    -- Count the number of on elements in the cube tree.
--
--    countOn :: P22CubeTree -> Integer
--    countOn (P22CubeTree cube@(P22CubeData switchedOn xRange yRange zRange) [])
--      | switchedOn = totalElements cube
--      | otherwise  = 0
--    countOn (P22CubeTree cube@(P22CubeData switchedOn _ _ _) subTrees)
--      | switchedOn = totalElements cube - (countOfSubTrees - onInSubTrees)
--      | otherwise  = onInSubTrees
--      where
--        onInSubTrees    = (sum . map countOn) subTrees
--        countOfSubTrees = (sum . map (totalElements . d_cubeData)) subTrees
--
--    -- Return the total number of elements in the cube.
--
--    totalElements :: P22CubeData -> Integer
--    totalElements P22CubeData _ (xLow, xHigh) (yLow, yHigh) (zLow, zHigh)
--      = fromIntegral $ (xHigh - xLow) * (yHigh - yLow) * (zHigh - zLow)
--
----    totalElements :: P22IndexRange -> P22IndexRange -> P22IndexRange -> Integer
----    totalElements (xLow, xHigh) (yLow, yHigh) (zLow, zHigh)
----      = fromIntegral $ (xHigh - xLow) * (yHigh - yLow) * (zHigh - zLow)
----
----    -- Here it is assumed that the range of the cube data entirely fits into the top level of the
----    -- cube tree.
----
----    addCubeToTree :: P22CubeTree -> P22CubeData -> P22CubeTree
----
----    -- Here we have the case where there are no sub-cubes in the tree.
----
----    addCubeToTree tree@(P22CubeTree (P22CubeData tOn tXRange tYRange tZRange) [])
----                  cubeData@(P22CubeData dOn dXRange dYRange dZRange)
----
----      -- If both cubes have their elements set to the same state, then there is no need to add a
----      -- sub-cube, so just return the original tree.
----
----      | tOn == dOn = tree
----
----      -- If the cube is the same size, but the on or off is different just replace the on value in
----      -- the original tree.
----
----      | tXRange == dXRange && tYRange == dYRange && tZRange == dZRange
----        = P22CubeTree (P22CubeData dOn tXRange tYRange tZRange) []
----
----      -- In this case, just add a sub-cube to the tree.
----
----      | otherwise = let subCube = convCubeDataToCubeTree cubeData
----                    in  P22CubeTree (P22CubeData tOn tXRange tYRange tZRange) [subCube]
----
----    -- Here is the more complicated case where there are sub-cubes in the tree, and we have to
----    -- figure out if the added cube intersects with any of these, and if so, reconfigure
----    -- them. Ultimately, the result will be to return the same cube tree with a new list of
----    -- sub-cubes. This is a complicated process depending on how the cube intersects sub-cubes,
----    -- whether the cube is set to on or off, and especially if the cube intersects multiple
----    -- sub-u=cubes.
----
----    addCubeToTree cubeTree newCube = cubeTree { d_subCubes = newSubCubes }
----      where
----        newSubCubes = mergeNewCubeWithSubCubes newCube (d_subCubes cubeTree)
----        treeOnOff   = d_on (d_cubeData cubeTree)
----
----        -- Merge the given cube into the list of cube trees.
----
----        mergeNewCubeWithSubCubes :: P22CubeData -> [P22CubeTree] -> [P22CubeTree]
----        mergeNewCubeWithSubCubes cubeData cubeTrees
----
----          -- If the new cube to add doesn't intersect with any sub-cubes, then either add it to the
----          -- list of sub-cubes or, if its on/off setting is the same as the current cube tree, just
----          -- return the existing sub-cube list, since the added cube does nothing.
----
----          | null intersectingSubCubes = if (d_on cubeData) == treeOnOff then cubeTrees
----                                        else newCubeTree : cubeTrees
----
----          -- If the cube intersects with one or more sub-cubes then it's much more complicated.  If
----          -- the on/off state of the added cube is the same as the current tree, then it will just
----          -- clip out chunks of existing sub-cubes, but if it has the opposite on/off state, then we
----          -- concatenate the new cube parts that didn't intersect with sub-cubes, new cube parts
----          -- that did intersect with existing sub-cubes, and then sub-cube parts that didn't
----          -- intersect with the added cube, and finally sub-cubes that didn't intersect at all with
----          -- the added cube.
----
----          | d_on cubeData == treeOnOff = separateFromCube ++ nonIntersectingSubCubes
----
----          | otherwise = newFromCube ++ intersectingWithCube ++ separateFromCube
----                        ++ nonIntersectingSubCubes
----          where
----            (newFromCube, intersectingWithCube, separateFromCube)
----              = mergeCubeWithSubCubes [] [newCubeTree] intersectingSubCubes
----
----            -- Go through the list of sub-cubes splitting them between those that intersect the cube
----            -- being added and those that don't.
----
----            (intersectingSubCubes, nonIntersectingSubCubes)
----              = (foldr splitIntersectingAndNon ([], []) . map (calcIntersectTree cubeData)) cubeTrees
----
----            -- Create a cube tree from the cube data passed in.
----
----            newCubeTree = convCubeDataToCubeTree cubeData
----
----            splitIntersectingAndNon (iTypeM, cubeTree) (inter, nonInter)
----              | isNothing iTypeM = (inter, cubeTree : nonInter)
----              | otherwise = (cubeTree : inter, nonInter)
----
----    mergeCubeWithSubCubes :: [P22CubeTree] -> [P22CubeTree] -> [P22CubeTree]
----                             -> ([P22CubeTree], [P22CubeTree], [P22CubeTree])
----    mergeCubeWithSubCubes mergedSubTrees cubePieces remainingSubTrees = go possibleIntersections
----      where
----        possibleIntersections = [(calcIntersect (fst3 x) (fst3 y), x, y)
----                                  | x <- cubeIndivid, y <- subTreeIndivid]
----        fst3 (i, _, _) = i
----        cubeIndivid = eachElement cubePieces
----        subTreeIndivid = eachElement remainingSubTrees
----
----        go :: [(Maybe P22IntersectType, (P22CubeTree, [P22CubeTree], [P22CubeTree]), (P22CubeTree, [P22CubeTree], [P22CubeTree]))]
----              -> ([P22CubeTree], [P22CubeTree], [P22CubeTree])
----        go [] = (mergedSubTrees, cubePieces, remainingSubTrees)
----        go ((Nothing, _, _) : remainder) = go remainder
----        go ((Just interType, (cubeTree, cubeTrees1, cubeTrees2), (subTree, subTrees1, subTrees2)) : _)
----          = mergeCubeWithSubCubes (mergedSubTrees ++ intersectTrees)
----                                  (cubeTrees1 ++ cubeTrees2 ++ onlyCubeTrees)
----                                  (subTrees1 ++ subTrees2 ++ onlySubTrees)
----          where
----            (intersectTrees, onlyCubeTrees, onlySubTrees) = intersectCubes interType cubeTree subTree
----
----    -- This function takes two cube trees and an intersection type that defines how these cubes
----    -- intersect. It returns three lists: 1) cube trees defining the intersection, 2) cube trees
----    -- only in the first cube tree passed in, and 3) cube trees only in the second cube tree passed
----    -- in. The results will also correctly contain the sub-cubes.
----
----    intersectCubes :: P22IntersectType -> P22CubeTree -> P22CubeTree
----                      -> ([P22CubeTree], [P22CubeTree], [P22CubeTree])
----    intersectCubes intersectType cubeTree1 cubeTree2 = undefined
----
----    -- Return a list with each element a triple with a single element, those before it and those
----    -- after it. Allows access to each item of the list while the other items in the list are still
----    -- available.
----
----    eachElement :: [a] -> [(a, [a], [a])]
----    eachElement xs = go [] xs
----      where
----        go prior [] = []
----        go prior (y : ys) = (y, prior, ys) : go (y : prior) ys
----
----    -- Convert a cube data to a cube tree with no sub-cube trees.
----
----    convCubeDataToCubeTree :: P22CubeData -> P22CubeTree
----    convCubeDataToCubeTree cubeData = P22CubeTree cubeData []
----
----    -- Return how the cubes interact, or of they are entirely separate, then Nothing.
----
----    calcIntersectTree :: P22CubeData -> P22CubeTree -> (Maybe P22IntersectType, P22CubeTree)
----    calcIntersectTree cubeData cubeTree
----      = (calcIntersect (convCubeDataToCubeTree cubeData) cubeTree, cubeTree)
----
----    -- Given two trees, determine if they intersect or not, and if so, in what way.
----
----    calcIntersect :: P22CubeTree -> P22CubeTree -> Maybe P22IntersectType
----    calcIntersect cubeData cubeTree = undefined
----
----    -- Count the number of on elements in the cube tree.
----
----    countOn :: P22CubeTree -> Integer
----    countOn (P22CubeTree (P22CubeData switchedOn xRange yRange zRange) [])
----      = if switchedOn then totalElements xRange yRange zRange else 0
----    countOn (P22CubeTree (P22CubeData switchedOn xRange yRange zRange) subTrees) = undefined
----
----    totalElements :: P22IndexRange -> P22IndexRange -> P22IndexRange -> Integer
----    totalElements (xLow, xHigh) (yLow, yHigh) (zLow, zHigh)
----      = fromIntegral $ (xHigh - xLow) * (yHigh - yLow) * (zHigh - zLow)
----
----    -- Create an assiciation list for this cube.
----
----    switchAllCubeElements :: P22CubeData -> [((Int, Int, Int), Bool)]
----    switchAllCubeElements (P22CubeData onOff (xLow, xHigh) (yLow, yHigh) (zLow, zHigh))
----      = [((x, y, z), onOff) | x <- [xLow..xHigh], y <- [yLow..yHigh], z <- [zLow..zHigh]]
----
--    -- Returns true if all of the bounds of this cube are within the -50..50 region, needed for part
--    -- 1 of this problem.
--
--    inRangeBounds :: P22CubeData -> Bool
--    inRangeBounds (P22CubeData _ (xLow, xHigh) (yLow, yHigh) (zLow, zHigh))
--      = all inRangeSingle [xLow, xHigh, yLow, yHigh, zLow, zHigh]
--      where
--        inRangeSingle :: Int -> Bool
--        inRangeSingle val = val >= -50 && val <= 50
--
--    -- Parse the input.
--
--    parseCubeRanges :: Parser [P22CubeData]
--    parseCubeRanges = many parseCubeData
--      where
--        parseCubeData :: Parser P22CubeData
--        parseCubeData = do
--          onOff <- parseOnOff
--          _ <- space
--          xLowHigh <- parseRange
--          _ <- symbol ","
--          yLowHigh <- parseRange
--          _ <- symbol ","
--          zLowHigh <- parseRange
--          _ <- space
--          return (P22CubeData onOff xLowHigh yLowHigh zLowHigh)
--          where
--
--            -- Parse the word on or off and return the corresponding boolean value.
--
--            parseOnOff :: Parser Bool
--            parseOnOff = do
--                _ <- symbol "on"
--                return True
--              <|> do
--                _ <- symbol "off"
--                return False
--
--            -- Parse an index range (low, high), matching for "x=", "y=", or "z=" at the start.
--
--            parseRange :: Parser P22IndexRange
--            parseRange = do
--              _ <- sat (\c -> c == 'x' || c == 'y' || c == 'z')
--              _ <- symbol "="
--              lowVal <- int
--              _ <- symbol ".."
--              highVal <- int
--              return (lowVal, highVal)
          
puzzle_22 :: IO Int
puzzle_22 = do
  parseRes <- fmap (parse parseCubeRanges) (readFile "puzzle_22.inp")

  -- Standard error check to see that there was exactly one full parser interpretation of the input.
  
  when (badParseOfInput parseRes)
       (ioError $ userError "Parse error in input for puzzle 22.")

  -- Read the list of cube definitions.

  let cubeDefinitions = (fst . head) parseRes

      -- Define the maximum cube size.

      (xLow, xHigh) = (-50, 50)
      (yLow, yHigh) = (-50, 50)
      (zLow, zHigh) = (-50, 50)

      -- Fill in a three-dimensional array of Boolean values, one cube after the other, then count
      -- the elements set to true.

      finalArr :: UA.Array (Int, Int, Int) Bool
      assocList = (concatMap switchAllCubeElements . filter inRangeBounds) cubeDefinitions
      finalArr = UA.accumArray (\_ x -> x) False ((xLow, yLow, zLow), (xHigh, yHigh, zHigh))
                 assocList
      onCount = (length . filter id . UA.elems) finalArr

  return onCount

  where

    -- Create an assiciation list for this cube.

    switchAllCubeElements :: P22CubeData -> [((Int, Int, Int), Bool)]
    switchAllCubeElements (P22CubeData onOff (xLow, xHigh) (yLow, yHigh) (zLow, zHigh))
      = [((x, y, z), onOff) | x <- [xLow..xHigh], y <- [yLow..yHigh], z <- [zLow..zHigh]]

    -- Returns true if all of the bounds of this cube are within the -50..50 region.

    inRangeBounds :: P22CubeData -> Bool
    inRangeBounds (P22CubeData _ (xLow, xHigh) (yLow, yHigh) (zLow, zHigh))
      = all inRangeSingle [xLow, xHigh, yLow, yHigh, zLow, zHigh]
      where
        inRangeSingle :: Int -> Bool
        inRangeSingle val = val >= -50 && val <= 50

    -- Parse the input.

    parseCubeRanges :: Parser [P22CubeData]
    parseCubeRanges = many parseCubeData
      where
        parseCubeData :: Parser P22CubeData
        parseCubeData = do
          onOff <- parseOnOff
          _ <- space
          xLowHigh <- parseRange
          _ <- symbol ","
          yLowHigh <- parseRange
          _ <- symbol ","
          zLowHigh <- parseRange
          _ <- space
          return (P22CubeData onOff xLowHigh yLowHigh zLowHigh)
          where

            -- Parse the word on or off and return the corresponding boolean value.

            parseOnOff :: Parser Bool
            parseOnOff = do
                _ <- symbol "on"
                return True
              <|> do
                _ <- symbol "off"
                return False

            -- Parse an index range (low, high), matching for "x=", "y=", or "z=" at the start.

            parseRange :: Parser P22IndexRange
            parseRange = do
              _ <- sat (\c -> c == 'x' || c == 'y' || c == 'z')
              _ <- symbol "="
              lowVal <- int
              _ <- symbol ".."
              highVal <- int
              return (lowVal, highVal)
          
main :: IO ()
main = do

  startTime <- getTime Realtime

  -- Generate the results for each problem and check for the expected answer. The result will
  -- contain not only the result, but the time taken to compute it.

  computeCheckAndPrint2   puzzle_01 " 1" (1766, 1797)
  computeCheckAndPrint2   puzzle_02 " 2" (1507611, 1880593125)
  computeCheckAndPrint2   puzzle_03 " 3" (2003336, 1877139)
  computeCheckAndPrint2   puzzle_04 " 4" (35711, 5586)
  computeCheckAndPrint2   puzzle_05 " 5" (8350, 19374)
  computeCheckAndPrint2   puzzle_06 " 6" (372984, 1681503251694)
  computeCheckAndPrint2   puzzle_07 " 7" (349769, 99540554)
  computeCheckAndPrint2   puzzle_08 " 8" (543, 994266)
  computeCheckAndPrint2   puzzle_09 " 9" (452, 1263735)
  computeCheckAndPrint2   puzzle_10 "10" (388713, 3539961434)
  computeCheckAndPrint2   puzzle_11 "11" (1613, 510)
  computeCheckAndPrint2   puzzle_12 "12" (5756, 144603)
  computeCheckAndPrint2IS puzzle_13 "13" (720, "AHPRPAUZ")
  computeCheckAndPrint2   puzzle_14 "14" (2891, 4607749009683)
  computeCheckAndPrint2   puzzle_15 "15" (393, 2823)
  computeCheckAndPrint2   puzzle_16 "16" (943, 167737115857)
  computeCheckAndPrint2   puzzle_17 "17" (2628, 1334)
  computeCheckAndPrint2   puzzle_18 "18" (4202, 4779)
  computeCheckAndPrint2   puzzle_19 "19" (318, 12166)
  computeCheckAndPrint2   puzzle_20 "20" (5249, 15714)
  computeCheckAndPrint2   puzzle_21 "21" (918081, 158631174219251)
  computeCheckAndPrint1   puzzle_22 "22" 647076

  -- Report on the time taken by all of the solutions together.

  endTime <- getTime Realtime
  let diff = computeElapsedTime (startTime, endTime)
      diffStr = printf "%0.5f sec" diff

  putStrLn $ mconcat ["\nTotal time for all solutions: ", diffStr]
