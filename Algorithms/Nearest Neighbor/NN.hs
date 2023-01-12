import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Label = Char
type Coordinates = [Float]
type Point = (Coordinates, Label)

distanceType = 1
classSeperator = '$'

-- Reads in the file
intakeFile :: FilePath -> [Coordinates] -> IO [Point]
intakeFile filePath points = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod points) parsedInput
    parsedInput = fmap (parseFile) rawLines
    rawLines = fmap (lines) (readFile filePath)


--parses the lines from the input file
parseFile :: [String] -> [Point]
parseFile rawLines = recurseLines 'A' rawLines

-- Goes through each line of the file, converting it into a point
-- If it reaches a class seperator, it increments the label and continues
recurseLines :: Char -> [String] -> [Point]
recurseLines label [] = []
recurseLines label (lineHead:lineTail) | headChar == classSeperator = recurseLines newLabel lineTail
                                       | otherwise = linePoint : recurseLines label lineTail
  where
    linePoint = parsePoint label lineHead
    headChar = head lineHead
    newLabel = succ label


-- Converts a line into its respective point
parsePoint :: Label -> String -> Point
parsePoint label rawLine = point
  where
    point = (position, label)
    position = map (getInt) splitLine
    getInt value = read (value) :: Float
    splitLine = words (map (nonDigit) rawLine)
    nonDigit = (\x -> if or [isDigit x, isMinus x] then x else ' ')
      where
        isMinus x' = x' == '-'


-- Main method
mainMethod :: [Coordinates] -> [Point] -> [Point]
mainMethod pointsToClassify dataSet = map (classifyPoint dataSet) pointsToClassify


-- Classifies a single point
classifyPoint :: [Point] -> Coordinates -> Point
classifyPoint dataSet pointToClassify = (pointToClassify, closetLabel)
  where
    closetLabel = snd closestPoint
    closestPoint = fst $ minimumBy (\x y -> compare (snd x) (snd y)) zippedDistances
    zippedDistances = zip dataSet allDistances
    allDistances = map (measureDistance pointToClassify) dataSet

-- Measures euclidean distance
measureDistance :: Coordinates -> Point -> Float
measureDistance coord1 (coord2,_) = euclidDist
  where
    euclidDist = sqrt sumSquare
    sumSquare = sum squareAbs
    squareAbs = map (**2) absDelta
    absDelta = map (abs) delta
    delta = zipWith (-) coord1 coord2



