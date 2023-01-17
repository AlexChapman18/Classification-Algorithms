import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type Unit = Float
type Line = [Unit]
type Class = Char
type Position = [Unit]
type Point = (Class, Position)
type Points = [Point]
type Itteration = Int
type Classification = Bool
type Classifications = [Classification]

-- Variable
learningRate = 0.5


-- Reads in the file
intakeFile :: FilePath -> IO (Maybe Line)
intakeFile filePath = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod) allPoints
    allPoints = fmap (parseAllPoints) rawPoints
    rawPoints = fmap (break ("-"==)) rawLines
    rawLines = fmap (lines) (readFile filePath)


--parses the lines from the input file
parseAllPoints :: ([String],[String]) -> Points
parseAllPoints rawPoints = classA ++ classB
  where
    classA = map (lineToPoint 'A') (fst rawPoints)
    classB = map (lineToPoint 'B') (tail (snd rawPoints))


-- Converts a line into its respective point
lineToPoint :: Char -> String -> Point
lineToPoint pClass line = point
  where
    point = (pClass, position ++ [1.0])
    position = map (getInt) splitLine
    getInt value = read (value) :: Float
    splitLine = words (map parseChar line)
    parseChar = (\x -> if or [isDigit x, isMinus x] then x else ' ')
      where
        isMinus x' = x' == '-'


-- Main method
mainMethod :: Points -> Maybe Line
mainMethod allPoints = findClassifier 0 initialLine allPoints
  where
--    initialLine = take numDimensions (repeat 1.0)
    initialLine = [1.0,1.0,-2]
    numDimensions = (length.snd.head) allPoints


--Attempts to find a classifier, maximum 1000 alterations
findClassifier :: Itteration -> Line -> Points -> Maybe Line
findClassifier 1000 _ _ = Nothing
findClassifier iter line points | isCorrect = Just line
                                | otherwise = traceShow (line, classifications) findClassifier (iter+1) newLine points
  where
     newLine = getNewLine line points classifications
     isCorrect = and classifications
     classifications = classifyPoints line points


--Generates a new line based on wrong classifications
getNewLine :: Line -> Points -> Classifications -> Line
getNewLine curLine points classifications = foldl (applyAlter) curLine filtPoints
  where
    filtPoints = map snd $ filter (not.fst) $ zip classifications points


--Alters the line by a point
applyAlter :: Line -> Point -> Line
applyAlter line (pClass, position) | (pClass == 'A') = (zipWith (+) line lRatePosition)
                                   | otherwise = (zipWith (-) line lRatePosition)
  where
    lRatePosition = map (learningRate*) position


-- classifies multiple points
classifyPoints :: Line -> Points -> Classifications
classifyPoints line [] = []
classifyPoints line (headPoint:tailPoints) = classification : classifyPoints line tailPoints
  where
    classification = classifyPoint line headPoint


-- Classifies a single point
classifyPoint :: Line -> Point -> Bool
classifyPoint line (pClass, position) | and [pClass == 'A', classification > 0] = True
                                      | and [pClass == 'B', classification < 0] = True
                                      | otherwise = False
  where
    classification = sum zipMulti
    zipMulti = zipWith (*) line position